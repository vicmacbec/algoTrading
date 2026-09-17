# Instrucciones de comportamiento

## Antes de cualquier cambio no trivial
- Activa modo plan (investiga antes de ejecutar). Lee a profundidad: código relevante,
  `docs/estructura-y-flujo.md`, `docs/arquitectura.md` y `docs/reglas-negocio.md` si existen.
- Si detectas otra área de oportunidad fuera del alcance pedido, menciónala explícitamente
  y pregunta si se incorpora ahora o se deja para después (no la implementes sin confirmar).
- No asumas nada que no esté explícito en la petición. Si hay ambigüedad, pregunta antes
  de escribir código.
- Si consideras que un cambio solicitado no es adecuado (rompe una convención, contradice
  la arquitectura, introduce riesgo), dilo y explica por qué no lo vas a agregar tal cual,
  antes de proceder.

## Alcance
- Solo modifica lo que fue solicitado explícitamente. No "aproveches" para tocar código,
  nombres, formato o estructura que no se pidió, salvo que el usuario lo indique.

## Flujo de cambios de código
- Para cualquier cambio de código no trivial, sigue el flujo definido en el comando
  `/implementar-cambio` (`.claude/commands/implementar-cambio.md`).
- **Carril rápido:** si el cambio es de una sola línea o es solo texto (typo, copy,
  comentario, valor de config trivial), omite el paso de plan y la entrada en
  `docs/historial-cambios.md` — actualiza únicamente el archivo de `docs/` directamente
  afectado por ese cambio, si alguno lo está, e implementa directo. Ante la duda de si el
  cambio califica, trátalo como cambio normal (con plan y changelog).

## Estructura y documentación
- Aplica siempre la convención de carpetas descrita en la skill `project-structure`.
- Mantén `docs/` sincronizado según la skill `docs-maintainer` — nunca dejes un cambio
  sin reflejar en la documentación correspondiente (salvo carril rápido, ver arriba).
- Si el proyecto usa agentes (LLM orquestando tareas), aplica la skill `agent-observability`
  para trazabilidad de SQL, tokens/costo, uso de agentes y tiempos de ejecución.

## Commits
- Pregunta en qué rama debe ir el commit **solo si** no se indicó una rama en la petición
  y no hay una rama de trabajo activa distinta de la rama principal (`main`/`master`).
  Si ya estás en una rama de feature activa, asume que el commit va ahí sin preguntar.
- Pregunta si corresponde hacer merge a otra rama únicamente cuando el cambio cierre una
  unidad de trabajo completa (no en cada commit individual).
- Nunca hagas `git push` sin confirmación explícita.
