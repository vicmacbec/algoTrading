# Historial de cambios

Changelog corto: una entrada por cambio significativo, con el hash del commit. El detalle de
implementación vive en el commit, no aquí.

## [b50df3c] 2026-09-17
El snapshot gana destino S3 y `lambda_handler`, y se descubre que **Binance responde HTTP 451 a
las IPs de Estados Unidos**: la función desplegada en `us-east-2` no pudo descargar ni un
endpoint. Una sonda desde esa región confirmó que los dumps históricos y el spot público sí son
alcanzables, pero los futuros no, así que el cómputo se mueve a `mx-central-1`. Se añaden el
script de despliegue idempotente y el de sondeo de región, y las políticas dejan de fijar la
región en sus ARNs.

## [d0acf6e] 2026-09-17
Arranca el sistema cuantitativo en Python (entorno con uv sobre 3.12, captura diaria del
universo point-in-time por cron, primera suite de pruebas) y se archiva el código R en
`src/legacy_r/` — el edge de la estrategia anterior resultó del mismo orden de magnitud que su
costo y el modelo de ML tenía fugas que invalidaban sus métricas, así que se reconstruye en vez
de parchearse. El snapshot diario se instala primero porque es el único dato que no se puede
reconstruir hacia atrás.

## [2edc8a5] 2026-09-17
Se agregó `keys.R` al `.gitignore` en sus dos rutas posibles (`Scripts/` y `src/`), en las tres
ramas — tras el renombre de la carpeta, cada rama ignoraba solo una de ellas, así que al cambiar
de rama el archivo de credenciales quedaba visible y podía subirse por error.

## [3b81b8a] 2026-09-17
Se integró `developments` en esta rama para incorporar el commit `f8fd03b` (ejecución del modelo
de ML sobre los datos recientes), hecho después de que esta rama se creó. El archivo llegó a su
ruta nueva, `src/Strategies/ml_tradingRules.R`, por detección de renombre.

## [aad8789] 2026-09-16
Se sustituyó `docs/estructura-del-proyecto.md` por los 6 archivos fijos de la convención —
la documentación estaba en un solo archivo que mezclaba estructura, arquitectura, reglas de
negocio y operación, y no había dónde registrar pendientes ni cambios.

## [cdbe1b7] 2026-09-16
Se renombró `Scripts/` a `src/` y se actualizaron las rutas del wrapper `.sh`, del script
productivo, del `.gitignore` y del README — para adoptar la convención de carpetas de la skill
`project-structure`. El despliegue de la EC2 y su crontab quedan pendientes.

## [c65193a] 2026-09-16
Se agregaron `logs/`, `.claude/settings.local.json` y `CLAUDE.local.md` al `.gitignore` —
son artefactos de runtime y configuración personal que no deben compartirse en el repo.

## [c8a3390] 2026-09-16
Se agregaron `CLAUDE.md` y `.claude/` con skills, comando, subagentes y hooks — para fijar el
flujo de trabajo con Claude Code (plan previo, alcance acotado, docs sincronizados, stats por
commit y confirmación antes de `git push`).
