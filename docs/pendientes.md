# Pendientes

- [ ] Apagar el cron local tras dos semanas de traslape con la Lambda
      Por qué falta: el snapshot es el único dato irrecuperable del proyecto; conviene tener dos
      fuentes hasta comprobar que la nube no falla. La Lambda arrancó el 2026-09-18.
      Por qué debe hacerse: mantener dos capturas indefinidamente duplica el punto de fallo
      humano (olvidar cuál es la buena) sin aportar nada una vez validada la Lambda. Antes de
      apagarlo hay que verificar que no haya huecos y subir a S3 lo que solo exista en local; el
      procedimiento está en [`operacion.md`](operacion.md).

- [ ] Apagar el cron de la EC2 y decidir el destino de la instancia
      Por qué falta: la instancia (`~/algoTrading/`) sigue con un crontab que apunta a rutas
      previas a la migración y opera pares BUSD que ya no reciben datos.
      Por qué debe hacerse: es un job que solo puede fallar o, peor, operar con datos
      congelados. El sistema nuevo corre en local durante las fases de investigación y en
      `mx-central-1` lo que debe correr sin la laptop, así que la instancia no hace falta.

- [ ] Reescribir el README
      Por qué falta: describe el proyecto de 2022 —estrategias en R contra pares BUSD— que ya
      está congelado, y lista dos estrategias con un nombre que ni siquiera coincide con el
      archivo real.
      Por qué debe hacerse: es lo primero que ve cualquiera que abra el repo, incluido tu yo de
      dentro de seis meses. Debe explicar el objetivo actual, el estado por fases y cómo
      arrancar el entorno.

- [ ] Backfill histórico a Parquet
      Por qué falta: el ingestor (`src/data/binance_vision.py`) ya sabe listar, descargar y
      normalizar, pero todavía no existe el proceso que recorre el universo y deja el panel
      escrito en `data/`.
      Por qué debe hacerse: es lo que desbloquea la Fase 2, que es el primer gate real del
      proyecto. Se empieza acotado al top 30 por volumen para llegar a las baselines en días en
      vez de semanas; escalar después es cambiar una lista.

- [ ] Reconstruir el universo point-in-time
      Por qué falta: la señal existe —el primer y el último mes con archivo en el bucket marcan
      el listado y el delisting de cada par— pero falta cruzarla con los snapshots diarios y
      dejarla como tabla consultable.
      Por qué debe hacerse: sin ella cualquier backtest tiene sesgo de supervivencia, porque el
      universo se elegiría con los pares que siguen vivos hoy.
