# Pendientes

- [ ] Ampliar `AlgoTradingPolicy` para que no fije la región **(bloquea la migración)**
      Por qué falta: la política adjunta al usuario `algoTrading` todavía ancla los ARNs de
      Lambda, Logs y Scheduler a `us-east-2`, así que crear la función en `mx-central-1` se
      deniega. El archivo del repo ya está corregido con `arn:aws:lambda:*:...:algotrading-*`.
      Por qué debe hacerse: es el único paso que impide migrar la captura fuera de EE.UU., y
      solo se puede hacer desde la consola de IAM: el usuario no tiene permisos sobre políticas.

- [ ] Migrar el snapshot a `mx-central-1` y desmantelar `us-east-2`
      Por qué falta: depende del punto anterior. La función de `us-east-2` quedó creada pero es
      inútil (Binance responde 451 desde EE.UU.) y su schedule ya se eliminó para que no falle
      a diario.
      Por qué debe hacerse: mientras tanto la única captura activa es el cron local, que depende
      de que la laptop esté encendida. Pasos: correr `configs/probe-binance-region.sh
      mx-central-1`, luego `REGION=mx-central-1 ./configs/deploy-snapshot-lambda.sh`, y al final
      borrar la función de `us-east-2`.

- [ ] Apagar el cron local tras dos semanas de traslape con la Lambda
      Por qué falta: el snapshot es el único dato irrecuperable del proyecto; conviene tener dos
      fuentes hasta comprobar que la nube no falla.
      Por qué debe hacerse: mantener dos capturas indefinidamente duplica el punto de fallo
      humano (olvidar cuál es la buena) sin aportar nada una vez validada la Lambda.

- [ ] Borrar de Google Drive los archivos de credenciales viejos
      Por qué falta: las llaves nuevas ya viven en `~/.config/algotrading/.env` y están
      verificadas, pero `config.yml` y `Credentials/` siguen en `~/Drive`, que Google sincroniza
      y donde conserva historial de versiones.
      Por qué debe hacerse: son llaves rotadas, pero el hábito importa: mientras estén ahí,
      cualquier llave futura acabará en el mismo sitio. Hay que borrarlas **y** purgar el
      historial de versiones en Drive, no solo el archivo.

- [ ] Apagar el cron de la EC2 y decidir el destino de la instancia
      Por qué falta: la instancia (`~/algoTrading/`) sigue con un crontab que apunta a rutas
      previas a la migración y opera pares BUSD que ya no reciben datos.
      Por qué debe hacerse: es un job que solo puede fallar o, peor, operar con datos
      congelados. El sistema nuevo corre en local durante las fases de investigación y no
      necesita la instancia encendida.

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
