#!/usr/bin/env bash
# Despliega la Lambda del snapshot del universo y su schedule de EventBridge.
#
# Es idempotente: crea lo que falte y actualiza lo que exista, así que se puede
# correr las veces que haga falta. Requiere el perfil de AWS con la política de
# configs/iam/algotrading-deploy-policy.json y los dos roles ya creados.
#
#   ./configs/deploy-snapshot-lambda.sh
#
set -euo pipefail

PERFIL="${AWS_PROFILE:-algotrading}"
REGION="${REGION:-${AWS_DEFAULT_REGION:-mx-central-1}}"
FUNCION="algotrading-snapshot"
SCHEDULE="algotrading-snapshot-diario"
BUCKET="algotrading-vicmacbec-data"
PREFIJO="snapshots"
ROL_EXEC="algotrading-snapshot-exec"
ROL_SCHED="algotrading-scheduler-invoke"
FUENTE="src/data/snapshot_universe.py"
# 00:10 UTC diario. EventBridge programa en UTC, que es justo lo que se busca:
# el día del snapshot se nombra en UTC y deja de depender de la zona local.
CRON="cron(10 0 * * ? *)"

aws() { command aws --profile "$PERFIL" --region "$REGION" "$@"; }

echo "==> Identidad"
CUENTA=$(aws sts get-caller-identity --query Account --output text)
echo "    cuenta $CUENTA, región $REGION"

echo "==> Empaquetando $FUENTE"
ZIP=$(mktemp -d)/funcion.zip
python3 - "$FUENTE" "$ZIP" <<'PY'
import sys, zipfile
fuente, destino = sys.argv[1], sys.argv[2]
# El handler es snapshot_universe.lambda_handler: el módulo va en la raíz del zip.
with zipfile.ZipFile(destino, "w", zipfile.ZIP_DEFLATED) as z:
    z.write(fuente, "snapshot_universe.py")
print(f"    {destino}")
PY

echo "==> Grupo de logs y retención"
aws logs create-log-group --log-group-name "/aws/lambda/$FUNCION" 2>/dev/null || true
aws logs put-retention-policy --log-group-name "/aws/lambda/$FUNCION" --retention-in-days 14
echo "    /aws/lambda/$FUNCION, 14 días"

echo "==> Función Lambda"
ENV_VARS="Variables={SNAPSHOT_OUT=s3://$BUCKET/$PREFIJO}"
if aws lambda get-function --function-name "$FUNCION" >/dev/null 2>&1; then
    aws lambda update-function-code --function-name "$FUNCION" --zip-file "fileb://$ZIP" >/dev/null
    aws lambda wait function-updated --function-name "$FUNCION"
    aws lambda update-function-configuration --function-name "$FUNCION" \
        --timeout 120 --memory-size 512 --environment "$ENV_VARS" >/dev/null
    aws lambda wait function-updated --function-name "$FUNCION"
    echo "    actualizada"
else
    # Tras crear un rol, Lambda puede tardar unos segundos en poder asumirlo.
    for intento in 1 2 3 4 5; do
        if aws lambda create-function --function-name "$FUNCION" \
            --runtime python3.12 --architectures arm64 \
            --role "arn:aws:iam::$CUENTA:role/$ROL_EXEC" \
            --handler snapshot_universe.lambda_handler \
            --zip-file "fileb://$ZIP" --timeout 120 --memory-size 512 \
            --environment "$ENV_VARS" \
            --description "Snapshot point-in-time del universo de Binance" >/dev/null 2>&1; then
            echo "    creada"; break
        fi
        if [ "$intento" = 5 ]; then
            echo "    ERROR: no se pudo crear la función tras 5 intentos"
            aws lambda create-function --function-name "$FUNCION" \
                --runtime python3.12 --architectures arm64 \
                --role "arn:aws:iam::$CUENTA:role/$ROL_EXEC" \
                --handler snapshot_universe.lambda_handler \
                --zip-file "fileb://$ZIP" --timeout 120 --memory-size 512 \
                --environment "$ENV_VARS" || true
            exit 1
        fi
        echo "    esperando a que el rol sea asumible (intento $intento)"
        sleep 6
    done
    aws lambda wait function-active --function-name "$FUNCION"
fi

echo "==> Schedule de EventBridge ($CRON UTC)"
TARGET=$(cat <<JSON
{"Arn":"arn:aws:lambda:$REGION:$CUENTA:function:$FUNCION",
 "RoleArn":"arn:aws:iam::$CUENTA:role/$ROL_SCHED",
 "RetryPolicy":{"MaximumRetryAttempts":3,"MaximumEventAgeInSeconds":3600}}
JSON
)
if aws scheduler get-schedule --name "$SCHEDULE" >/dev/null 2>&1; then
    aws scheduler update-schedule --name "$SCHEDULE" \
        --schedule-expression "$CRON" --schedule-expression-timezone UTC \
        --flexible-time-window Mode=OFF --target "$TARGET" >/dev/null
    echo "    actualizado"
else
    aws scheduler create-schedule --name "$SCHEDULE" \
        --schedule-expression "$CRON" --schedule-expression-timezone UTC \
        --flexible-time-window Mode=OFF --target "$TARGET" \
        --description "Snapshot diario del universo de Binance" >/dev/null
    echo "    creado"
fi

echo "==> Invocación de prueba"
SALIDA=$(mktemp)
aws lambda invoke --function-name "$FUNCION" --payload '{"force":false}' \
    --cli-binary-format raw-in-base64-out "$SALIDA" --query 'StatusCode' --output text
echo "    respuesta: $(cat "$SALIDA")"

echo "==> Objetos en s3://$BUCKET/$PREFIJO/"
aws s3api list-objects-v2 --bucket "$BUCKET" --prefix "$PREFIJO/" \
    --query 'Contents[].[Key,Size]' --output text | tail -10

echo
echo "Listo. Para ver los logs de la próxima corrida:"
echo "  aws logs tail /aws/lambda/$FUNCION --follow --profile $PERFIL --region $REGION"
