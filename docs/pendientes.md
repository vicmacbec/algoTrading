# Pendientes

- [ ] Apagar el cron local tras dos semanas de traslape con la Lambda
      Por qué falta: el snapshot es el único dato irrecuperable del proyecto; conviene tener dos
      fuentes hasta comprobar que la nube no falla. La primera corrida programada que funcionó
      fue la del 2026-09-19 (la del 18 nunca corrió sola), así que el traslape cuenta desde ahí:
      hasta el 2026-10-03 como mínimo.
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

- [ ] Construir y persistir el catálogo completo del universo
      Por qué falta: la lógica ya está en `src/data/universe.py` y validada contra los 3705
      símbolos de un snapshot real, pero el catálogo en sí no existe todavía: enumerar los meses
      publicados de cada par exige una petición de listado por símbolo, unas 3700 en total.
      Por qué debe hacerse: es la tabla que responde "qué pares eran operables el 14 de marzo de
      2021", y sin ella cualquier backtest elige su universo con los pares que siguen vivos hoy.
      Conviene construirlo con caché y reanudación, y persistirlo en Parquet para no repetir el
      recorrido.

- [ ] Que el despliegue aplique las políticas de los roles desde el repo
      Por qué falta: `configs/deploy-snapshot-lambda.sh` crea la función y el schedule, pero las
      políticas de los dos roles se aplicaron a mano, una vez, al crearlos.
      Por qué debe hacerse: es la causa raíz de dos fallos silenciosos seguidos —logs mudos el 18 y
      ninguna corrida programada el 19—, ambos porque el documento de AWS se quedó atrás del repo.
      Si cada despliegue hiciera `put-role-policy` desde `configs/iam/`, esa deriva no podría
      volver a ocurrir.

- [ ] Registrar cada bloque de features con su hipótesis y contarlo para el DSR
      Por qué falta: los indicadores ya existen en `src/features/`, pero el contador de
      configuraciones probadas vive en el tracker, que es de la Fase 3 y todavía no existe.
      Por qué debe hacerse: la regla de añadir features por bloques con hipótesis previa solo
      protege contra el sobreajuste si cada bloque se cuenta. Sin el registro, la tentación de
      probar "solo uno más" no deja rastro y el Deflated Sharpe se calcula con un número falso.

- [ ] Resolver o documentar las 3 ambigüedades de nombre que quedan
      Por qué falta: `LUNAEUR`, `GALAEUR` y `ARBIDR` admiten dos particiones válidas cada una
      (`LUNA`+`EUR` y `LUN`+`AEUR`, ambas con activos que existen), así que ninguna regla basada
      en el nombre las resuelve. Hoy la heurística falla en 3 de 3705 (0.08 %).
      Por qué debe hacerse: no urge —el mapa autoritativo del snapshot las cubre todas—, pero si
      algún día aparece un par delistado con esta forma y sin snapshot, se partirá mal en
      silencio. Bastaría una lista de excepciones explícitas.
