#!/usr/bin/env bash
# Comprueba desde qué regiones de AWS es alcanzable Binance.
#
# Binance responde HTTP 451 ("Unavailable For Legal Reasons") a las IPs de
# EE.UU., así que una región equivocada rompe toda la ingesta. Este script
# despliega una Lambda desechable en la región indicada, prueba los hosts que
# el proyecto necesita y la borra.
#
#   ./configs/probe-binance-region.sh mx-central-1
#   ./configs/probe-binance-region.sh sa-east-1
#
set -euo pipefail

REGION="${1:?uso: $0 <region>}"
PERFIL="${AWS_PROFILE:-algotrading}"
FUNCION="algotrading-probe"
ROL="algotrading-snapshot-exec"

aws() { command aws --profile "$PERFIL" --region "$REGION" "$@"; }

CUENTA=$(aws sts get-caller-identity --query Account --output text)
TMP=$(mktemp -d)
# La sonda se borra pase lo que pase: no debe quedar basura en la cuenta.
trap 'aws lambda delete-function --function-name "$FUNCION" >/dev/null 2>&1 || true; rm -rf "$TMP"' EXIT

cat > "$TMP/probe.py" <<'PY'
import urllib.error, urllib.request

HOSTS = {
    "api.binance.com (spot)":         "https://api.binance.com/api/v3/exchangeInfo",
    "data-api.binance.vision (spot)": "https://data-api.binance.vision/api/v3/exchangeInfo",
    "fapi.binance.com (futuros)":     "https://fapi.binance.com/fapi/v1/exchangeInfo",
    "data.binance.vision (dumps)":    "https://data.binance.vision/data/spot/monthly/klines/BTCUSDT/4h/BTCUSDT-4h-2025-06.zip",
}


def lambda_handler(event, context):
    out = {}
    for nombre, url in HOSTS.items():
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "algotrading-probe/1.0"})
            with urllib.request.urlopen(req, timeout=8) as r:
                out[nombre] = f"{r.status} OK"
        except urllib.error.HTTPError as e:
            out[nombre] = f"{e.code} {e.reason}"
        except Exception as e:
            out[nombre] = f"error {type(e).__name__}"
    return out
PY

python3 -c "
import zipfile
with zipfile.ZipFile('$TMP/probe.zip','w',zipfile.ZIP_DEFLATED) as z:
    z.write('$TMP/probe.py','probe.py')
"

echo "==> sonda temporal en $REGION"
aws lambda create-function --function-name "$FUNCION" \
    --runtime python3.12 --architectures arm64 \
    --role "arn:aws:iam::$CUENTA:role/$ROL" \
    --handler probe.lambda_handler --zip-file "fileb://$TMP/probe.zip" \
    --timeout 90 --memory-size 128 >/dev/null
aws lambda wait function-active --function-name "$FUNCION"

SALIDA="$TMP/salida.json"
aws lambda invoke --function-name "$FUNCION" --cli-read-timeout 120 "$SALIDA" \
    --query StatusCode --output text >/dev/null

echo "==> alcanzabilidad desde $REGION:"
python3 -c "
import json
d = json.load(open('$SALIDA'))
for k, v in d.items():
    marca = 'OK ' if v.startswith('200') else '!! '
    print(f'  {marca} {k:32} {v}')
bloqueados = [k for k, v in d.items() if v.startswith('451')]
print()
print('  VEREDICTO: región usable' if not bloqueados else f'  VEREDICTO: {len(bloqueados)} host(s) bloqueados por geografía')
"
echo "==> la sonda se elimina automáticamente al salir"
