---
name: project-structure
description: Usa esta skill siempre que crees un proyecto nuevo, agregues un módulo nuevo,
  o reorganices carpetas. Define la estructura de carpetas obligatoria que todo proyecto
  de Víctor debe tener, incluyendo el caso de proyectos con agentes y con RAG estructurado.
---

# Estructura de carpetas obligatoria

Todo proyecto debe tener, como mínimo:

- `src/` — código fuente de la aplicación.
- `test/` — pruebas unitarias (un archivo de test por módulo relevante de `src/`).
- `docs/` — toda la documentación (ver skill `docs-maintainer` para su contenido exacto).

## Si el proyecto usa agentes (LLM orquestando tareas)
Agrega, dentro de `src/` (no en la raíz):
- `src/agentes/` — definición de cada agente (rol, prompt del sistema, herramientas que usa).
- `src/tools/` — implementación de las tools/funciones que los agentes pueden invocar.
- `src/services/` — lógica de negocio y de integración reutilizable que NO es una tool de
  agente ni un endpoint (clientes a servicios externos, acceso a datos, cálculos de
  negocio compartidos). Las tools en `src/tools/` deben ser wrappers delgados que llaman
  a `src/services/` — así la lógica se puede probar y reusar sin pasar por un agente.
- `src/prompts/` — prompts versionados (system prompts, few-shot examples), separados del
  código.
- `src/skills/` — capacidades reutilizables de los agentes (si aplica al framework usado,
  p. ej. Strands Agents).

Agrega también, en la raíz del proyecto (son artefactos de runtime/trazabilidad, no
código fuente, así que van fuera de `src/`):
- `logs/` — trazabilidad de ejecución de los agentes (ver skill `agent-observability` para
  el detalle de qué se registra en cada una). Subcarpetas obligatorias:
  - `logs/queries/` — queries SQL generados por los agentes, un archivo `.sql` por día.
  - `logs/tokens-costo/` — registro estructurado de tokens de entrada/salida y costo
    estimado por llamada al modelo.
  - `logs/uso-agentes/` — tiempo de ejecución y % de uso por agente/subagente.
  - `logs/tiempos-ejecucion/` — desglose de duración por etapa de cada ejecución
    end-to-end.
  - `logs/reportes-cron/` — histórico de los reportes periódicos de estatus generados por
    el cron.

**`logs/` nunca se commitea** — son artefactos de runtime con timestamp de ejecución, no
código fuente. Agrégalo al `.gitignore` del proyecto (`logs/`) la primera vez que crees
la carpeta.

## Si además el proyecto tiene un RAG estructurado (consulta datos tabulares/SQL)
Agrega, dentro de `src/` (igual que `agentes/`, `tools/`, etc. — son insumos que el
código consume directamente, no artefactos de runtime como `logs/`):
- `src/catalogos/` — un `.json` por tabla consumida, con: nombre de campo, tipo de dato,
  valores aceptados, ejemplo, definición de negocio, dependencias y con qué otras tablas
  se relaciona (llaves foráneas / joins típicos).
- `src/queries/` — ejemplos de las queries más relevantes o más usadas contra ese
  catálogo, con un comentario explicando qué responden. (Distinto de `logs/queries/`:
  ahí van los SQL generados en runtime por los agentes, no ejemplos de referencia.)

## Al aplicar esta skill
1. Verifica qué carpetas ya existen antes de crear nada.
2. Si falta alguna carpeta obligatoria para el tipo de proyecto detectado, créala (puede
   quedar vacía con un `.gitkeep` o un `README.md` breve explicando su propósito).
3. No propongas carpetas adicionales no listadas aquí sin preguntar primero.
