# Pendientes

- [ ] Actualizar el despliegue de la EC2 y su crontab tras la migración a `src/`
      Por qué falta: el renombre `Scripts/` → `src/` se hizo en el repo; la instancia
      (`~/algoTrading/`) y las entradas de `crontab -e` siguen apuntando a la ruta vieja.
      Por qué debe hacerse: la siguiente corrida programada falla con "No such file or
      directory" hasta que se haga `git pull` en la instancia y se corrija el crontab.

- [ ] Migrar los pares de BUSD a USDT o USDC
      Por qué falta: todos los scripts se escribieron en 2022, cuando BUSD era el par de
      referencia en Binance.
      Por qué debe hacerse: Binance descontinuó BUSD entre 2023 y 2024, así que hoy ninguna
      corrida obtiene datos nuevos y el proyecto entero no puede ejecutarse de verdad.

- [ ] Unificar las rutas de local y EC2 en un solo punto de configuración
      Por qué falta: se resolvió rápido comentando y descomentando líneas en el bloque
      `#### Paths ####` de cada script.
      Por qué debe hacerse: es la causa más probable de un fallo al desplegar; basta con
      olvidar una línea para que el script escriba en la ruta equivocada. Debería salir de
      `config.yml` o de una variable de entorno.

- [ ] Sacar las credenciales de texto plano
      Por qué falta: `config.yml` y `Credentials/` son la forma más simple y están ignorados
      por git.
      Por qué debe hacerse: las llaves de Binance permiten operar con dinero real. `src/keys.R`
      ya explora `keyring` como alternativa; falta terminarlo y rotar las llaves actuales.

- [ ] Corregir el encabezado de `src/Strategies/ml_tradingRules.R`
      Por qué falta: el archivo se creó copiando `MASlope_ATRStopL_Prod.R` y el encabezado
      nunca se reescribió.
      Por qué debe hacerse: dice "Productive", da un tiempo de ejecución de ~8 s y un ejemplo
      de `Rscript` que no corresponden al script de ML, lo que confunde sobre qué hace y cómo
      se ejecuta.

- [ ] Crear `test/` con pruebas unitarias
      Por qué falta: el proyecto nació como una serie de análisis exploratorios en RStudio, sin
      funciones aisladas que probar.
      Por qué debe hacerse: la skill `project-structure` lo exige y el flujo de
      `/implementar-cambio` incluye correr la suite. Los candidatos naturales son las funciones
      puras: `an()`, el cálculo de `maSlope`, las fórmulas de `realRate`/`cumYield` y el cálculo
      de cantidades válidas de `src/Tests/binance.R`.

- [ ] Completar el README
      Por qué falta: se escribió al inicio del proyecto, con dos estrategias listadas.
      Por qué debe hacerse: hoy hay ocho estrategias y el README solo menciona dos, una de
      ellas con un nombre (`pumpNGo.R`) que no coincide con el archivo real (`pumNGo.R`).

- [ ] Terminar el modelo de `ml_tradingRules.R` (issue #9)
      Por qué falta: está en desarrollo activo; ya tiene cross-validation, grid search y
      análisis de punto de corte.
      Por qué debe hacerse: es la línea de trabajo abierta del proyecto y todavía no produce
      una señal utilizable en el flujo de backtest ni en producción.
