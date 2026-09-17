---
name: docs-maintainer
description: Usa esta skill cada vez que termines un cambio de código, cierres un plan,
  o el usuario pida "actualiza la documentación". Define el conjunto fijo de archivos que
  vive en docs/ y las reglas para mantenerlos actualizados sin que la carpeta crezca sin
  control.
---

# Documentación del proyecto (`docs/`)

## Principio: optimizar cantidad de archivos
`docs/` debe tener un **número fijo y pequeño de archivos vivos** (los listados abajo).
Antes de crear un archivo nuevo en `docs/`, verifica si el contenido cabe en uno de los
existentes. Solo crea un archivo nuevo si el tema es genuinamente distinto a todos los de
la lista y se espera que crezca por sí solo.

## Archivos obligatorios y qué va en cada uno

- **`docs/reglas-negocio.md`** — reglas de negocio, definiciones de términos del dominio,
  métricas, fórmulas de cálculo, y ejemplos cuando el cálculo no sea obvio.

- **`docs/arquitectura.md`** — arquitectura del proyecto (componentes, cómo se comunican,
  diagrama en texto/mermaid si ayuda). Cuando un plan de cambios implique una decisión de
  arquitectura, agrega aquí un breve párrafo con la decisión tomada y por qué.

- **`docs/estructura-y-flujo.md`** — qué hace cada carpeta del repo, qué hacen los scripts
  más importantes, y una estimación del % de uso/ejecución de cada script si se conoce
  (p. ej. a partir de logs o de la frecuencia con la que se invoca). Actualízalo cuando se
  agregue, elimine o cambie de propósito una carpeta o script relevante.

- **`docs/operacion.md`** — cómo actualizar/ejecutar el proyecto en local y en productivo.
  Para proyectos con agentes en AWS Bedrock AgentCore, incluye como mínimo:
  - Comandos de `bedrock-agentcore-starter-toolkit` para actualizar el runtime del agente.
  - Comando y flags para hacer deploy.
  - Comando de `invoke` de prueba, explicando qué metadata se envía (session id, actor id,
    payload) y qué se espera de vuelta.
  - Si el proyecto usa otra nube, reemplaza esta sección por el equivalente del servicio
    correspondiente (p. ej. Vertex AI Agent Builder en GCP, Azure AI Foundry en Azure).

- **`docs/historial-cambios.md`** — changelog corto y conciso. Una entrada por cambio
  significativo, formato:
````
  ## [hash-del-commit] YYYY-MM-DD
  Qué cambió (1 línea) — Por qué (1-2 líneas)
````
  No repitas aquí el diff completo ni detalles de implementación; eso vive en el commit.

  **Archivado (única excepción a "6 archivos fijos"):** cuando `historial-cambios.md`
  supere ~30 entradas o 6 meses de antigüedad en su entrada más vieja (lo que se cumpla
  primero), mueve las entradas más viejas a `docs/historial-archivo/YYYY.md` (un archivo
  por año), y deja en `historial-cambios.md` solo las recientes.
  `docs/historial-archivo/` no cuenta contra el límite de archivos porque es archivado,
  no documentación viva — no se lee en el flujo normal de cambios, solo se consulta si
  alguien busca algo puntual del pasado lejano.
 
- **`docs/pendientes.md`** — lo que falta por cambiar/agregar, con la justificación de por
  qué falta y por qué debe hacerse. Formato por ítem:
````
  - [ ] Descripción del pendiente
        Por qué falta: ...
        Por qué debe hacerse: ...
````
  Cuando un pendiente deja de ser relevante (se implementó, o el usuario dice
  explícitamente que ya no se requiere), **elimínalo del archivo** — no lo marques como
  hecho y lo dejes ahí, no debe acumular ruido histórico (eso ya lo cubre
  `historial-cambios.md`).
 
## Regla de actualización
En cada cambio de código que hagas:
1. Identifica cuál(es) de los 6 archivos de arriba necesita(n) actualizarse.
2. Actualízalos como parte del mismo cambio (no lo dejes pendiente).
3. Si el cambio surgió de un plan con decisiones de diseño, agrega esas decisiones al
   archivo que más convenga (arquitectura → `arquitectura.md`, negocio → `reglas-negocio.md`, etc.),
   no crees un archivo de "decisiones" separado.
