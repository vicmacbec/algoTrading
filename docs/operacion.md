# Operación

## Requisitos

- R con los paquetes: `binancer`, `data.table`, `TTR`, `ggplot2`, `scales`, `stringr`, `zoo`,
  `plotly`, `purrr`, `roll`, `patchwork`, `lubridate`, `dplyr`, `config`, `keyring`, `aws.s3`.
  Para `ml_tradingRules.R`, además: `xgboost`, `caret`, `GGally`, `pROC`.
- `config.yml` en la raíz del proyecto (no versionado):

```yaml
default:
  Binance:
    key: <tu key>
    secret: <tu secret>
AWS:
  S3:
    AWS_ACCESS_KEY: <...>
    AWS_SECRET: <...>
    AWS_DEFAULT_REGION: 'us-east-2'
```

Los scripts lo leen con `config::get(value = "Binance", file = paste0(pathDataInYml, "config.yml"))`
y `config::get(config = "AWS", value = "S3")`.

## Local

Abre [`AlgoTrading.Rproj`](../AlgoTrading.Rproj) en RStudio y ejecuta el script por secciones
(`#### ... ####`). No hay un punto de entrada único: cada estrategia es un análisis
independiente que se corre a mano.

Antes de correr un script, revisa el bloque `#### Paths ####`: las rutas de local y de la EC2
conviven comentadas y hay que dejar activa la que corresponda.

| Entorno | Raíz |
|---|---|
| Local | `~/Drive/Codigos/AlgoTrading/` |
| EC2 | `~/algoTrading/` |

## Productivo (EC2 + cron)

El único script productivo es [`MASlope_ATRStopL_Prod.R`](../src/Strategies/MASlope_ATRStopL_Prod.R),
invocado por su wrapper:

```bash
sh ~/algoTrading/src/Strategies/MASlope_ATRStopL_Prod.sh \
  >> ~/algoTrading/DataOut/MASlope_ATRStopLoss/Logs/$(date '+%Y_%m_%d')_Trades.log
```

El wrapper llama al `.R` con el símbolo como argumento y guarda un log por corrida en
`DataOut/MASlope_ATRStopLoss/allLogs/`. El par se define dentro del `.sh` (variable `symbol`);
si el `.R` se invoca sin argumento, usa `FXSBUSD` por defecto.

Para agregar más pares, duplica el bloque de `symbol` en el `.sh`: el `.R` reescribe solo las
filas del par que recibe y conserva las demás.

La cadencia natural es cada 4 horas, alineada con el intervalo de las velas. El script calcula
cuánto falta para el siguiente cierre de vela con
`difftime(floor_date(Sys.time() + 4*3600, unit = "hour"), Sys.time(), units = "secs")`.

> **Pendiente tras la migración a `src/`:** la copia del repo en la EC2 (`~/algoTrading/`) y las
> entradas de crontab todavía apuntan a `~/algoTrading/Scripts/...`. Hay que hacer `git pull` en
> la instancia y revisar `crontab -e` antes de la siguiente corrida, o el job falla con "No such
> file or directory".

## Infraestructura AWS

Las guías completas están en `AWS/` (fuera de git):

- `r_Aws_Instructions_Ubuntu.md` — crear la EC2, instalar R y RStudio Server (puerto 8787),
  abrir el puerto en el security group, crear usuario y grupo, instalar git, configurar S3,
  encender y apagar la instancia con Lambda + EventBridge Scheduler, y el crontab.
- `r_Aws_Instructions.md` — lo mismo para Amazon Linux 2, más la migración de la instancia entre
  cuentas de AWS.

Copiar archivos entre local y la instancia:

```bash
scp -i <key.pem> <archivo> ubuntu@<ip>:~/algoTrading/     # local → EC2
scp -i <key.pem> ubuntu@<ip>:~/algoTrading/<archivo> .    # EC2 → local
```

## S3

Bucket `algotrading-vicmacbec` (región `us-east-2`). Ejemplos en
[`src/Tests/awsS3.R`](../src/Tests/awsS3.R):

```r
put_object(file = "<ruta local>", object = "<ruta en el bucket>", bucket = "algotrading-vicmacbec")
s3read_using(FUN = fread, object = "s3://algotrading-vicmacbec/Orders/allOrders_year_20220421.csv")
```

## Verificar una corrida

1. El log de `allLogs/` debe terminar en `#### End Script ####`.
2. `DataOut/MASlope_ATRStopLoss/Orders/allOrders_year_20220421.csv` debe traer la fecha de la
   última vela en `order_openTime` u `order_closeTime`.
3. Si el par no devolvió velas nuevas, el script no falla pero tampoco agrega filas: revisa
   primero que el par siga listado en Binance (ver la nota de BUSD en
   [`reglas-negocio.md`](reglas-negocio.md)).
