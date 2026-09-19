# Políticas IAM del proyecto

Los archivos `.json` de esta carpeta son documentos de política **listos para usar**: IAM
rechaza cualquier clave desconocida al nivel superior (`MalformedPolicyDocument`), así que no
llevan comentarios dentro. Toda la explicación vive aquí.

Antes de usarlos, sustituye `<ACCOUNT_ID>` por el identificador de la cuenta:

```bash
export ACCOUNT_ID=$(aws sts get-caller-identity --profile algotrading --query Account --output text)
export AWS_PROFILE=algotrading AWS_DEFAULT_REGION=us-east-2
sed -i "s/<ACCOUNT_ID>/$ACCOUNT_ID/g" configs/iam/*.json   # o hazlo en una copia temporal
```

## Qué es cada archivo

| Archivo | Para qué | Se adjunta a |
|---|---|---|
| `algotrading-deploy-policy.json` | Permite **crear y actualizar** la infraestructura del proyecto | El usuario IAM `algoTrading` (el de las llaves del CLI) |
| `lambda-trust-policy.json` | Deja que Lambda asuma el rol de ejecución | Rol `algotrading-snapshot-exec` (`--assume-role-policy-document`) |
| `lambda-execution-policy.json` | Lo único que la función necesita en runtime: escribir snapshots y loguear | Rol `algotrading-snapshot-exec` (política inline) |
| `scheduler-trust-policy.json` | Deja que EventBridge Scheduler asuma su rol | Rol `algotrading-scheduler-invoke` |
| `scheduler-invoke-policy.json` | Permite invocar **solo** la función del snapshot | Rol `algotrading-scheduler-invoke` (política inline) |

## Las dos cláusulas que de verdad importan

En `algotrading-deploy-policy.json`, `AdjuntarSoloPoliticaBasica` y `PasarSoloRolesDelProyecto`
son lo que impide una escalada de privilegios. Sin ellas, un usuario que pueda crear un rol,
adjuntarle **cualquier** política administrada y pasárselo a Lambda es **administrador de
facto**: se crea un rol con `AdministratorAccess`, se lo pasa a una función y ejecuta lo que
quiera. Por eso `iam:AttachRolePolicy` está condicionado a una única política
(`AWSLambdaBasicExecutionRole`) y `iam:PassRole` a los servicios `lambda` y `scheduler`.

## Correcciones encontradas probando permisos reales

La primera versión de estas políticas tenía dos huecos que solo aparecieron al ejercitarlas
contra la API:

1. **`scheduler:ListSchedules` y `scheduler:ListScheduleGroups` no admiten recurso.** Estaban
   atadas a un ARN con prefijo y devolvían `AccessDeniedException`. Van con `Resource: "*"`.
   Las que sí aceptan recurso (`CreateSchedule`, `GetSchedule`, `UpdateSchedule`,
   `DeleteSchedule`) siguen acotadas a `schedule/default/algotrading-*`.
2. **Faltaban las acciones `s3:Get*` de configuración** (`GetEncryptionConfiguration`,
   `GetBucketVersioning`, `GetBucketPublicAccessBlock`). Solo estaban las `Put*`, así que se
   podía configurar el bucket pero no auditarlo: cualquier verificación devolvía `AccessDenied`.
3. **El rol de ejecución necesita `s3:ListBucket`, aunque nunca liste nada.** Sin ese permiso, S3
   responde `403 Forbidden` —y no `404`— a un `HeadObject` sobre un objeto que no existe, para no
   revelar qué claves hay en el bucket. La comprobación de idempotencia tomaba ese 403 por fallo,
   así que toda corrida normal sobre un día nuevo fallaba. Va **sin condición de prefijo**: la
   petición `HeadObject` no lleva la clave `s3:prefix` en su contexto, así que una concesión
   condicionada no aplicaría.
4. **El rol del scheduler conservaba la región vieja** en su ARN tras migrar a `mx-central-1`: el
   archivo del repo se corrigió, pero en AWS seguía la versión original. Se aplicó con
   `put-role-policy` desde el archivo del repo.

La lección general: una política de mínimo privilegio no está terminada hasta que se ejercita
contra la API real. Probar solo las acciones de escritura deja ciegas las de lectura.

## Despliegue

```bash
# 1. Rol de ejecución de la Lambda
aws iam create-role --role-name algotrading-snapshot-exec \
  --assume-role-policy-document file://configs/iam/lambda-trust-policy.json
aws iam put-role-policy --role-name algotrading-snapshot-exec \
  --policy-name snapshot-runtime --policy-document file://configs/iam/lambda-execution-policy.json

# 2. Rol que usa EventBridge Scheduler para invocar
aws iam create-role --role-name algotrading-scheduler-invoke \
  --assume-role-policy-document file://configs/iam/scheduler-trust-policy.json
aws iam put-role-policy --role-name algotrading-scheduler-invoke \
  --policy-name invocar-snapshot --policy-document file://configs/iam/scheduler-invoke-policy.json
```

La función y el schedule se crean en el paso siguiente del plan, cuando el script tenga su
adaptador de S3.

## Verificar el bucket

`algotrading-vicmacbec-data` ya existe en `us-east-2` y acepta escritura en `snapshots/`. Con
los permisos `Get*` agregados, conviene confirmar que esté bien cerrado:

```bash
aws s3api get-public-access-block --bucket algotrading-vicmacbec-data
aws s3api get-bucket-encryption   --bucket algotrading-vicmacbec-data
```

Lo esperado: los cuatro valores de `PublicAccessBlockConfiguration` en `true` y cifrado
`AES256` como mínimo. Si alguno no lo está, se corrige con los `put-*` equivalentes.
