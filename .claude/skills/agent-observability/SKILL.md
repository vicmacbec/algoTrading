---
name: agent-observability
description: Usa esta skill cuando el proyecto orquesta agentes LLM (Bedrock/Strands,
  o equivalente en otra nube). Define qué trazabilidad debe implementarse en el código
  del proyecto (no en Claude Code) y las especificaciones del reporte periódico de estatus.
---

# Trazabilidad obligatoria en proyectos con agentes

Cuando implementes o modifiques un flujo de agentes, asegúrate de que el código del
proyecto (no Claude Code) registre lo siguiente:

## 0. Punto de entrada obligatorio (para que esto sea verificable, no solo "guía")
Toda llamada al modelo (Bedrock/Strands u otro) debe pasar por un único wrapper en
`src/services/observability.py`, p. ej. `invoke_agent_con_trazabilidad(...)`. Ese wrapper
es el único lugar del proyecto donde se registran tokens, costo y tiempos (puntos 2-4 de
abajo) — así queda en un solo sitio, se prueba una vez, y un cambio en el formato de log
no obliga a tocar cada agente.
**Nunca llames al cliente del modelo directamente desde `src/agentes/` o `src/tools/`**:
si necesitas invocar el modelo, pasa por el wrapper. Esto es lo que el hook
`check-agent-observability.sh` verifica automáticamente antes de cada commit.

## 1. Queries SQL generados
- Cada query SQL que un agente genere y ejecute (p. ej. contra Redshift) debe registrarse
  en un archivo `.sql` con timestamp y el agente/tool que lo generó como comentario:
```sql
  -- [2026-09-15T10:03:21Z] agente=orquestador_cdp tool=query_redshift
  SELECT ...
```
- Ubicación por defecto: `logs/queries/YYYY-MM-DD_HHMMSS.sql`, un archivo por ejecución
  (el timestamp es el de inicio de la corrida) — así no se sobrescribe ni se mezcla con
  otras ejecuciones del mismo día, salvo que el proyecto ya tenga una convención de
  logging distinta (revísala primero).

## 2. Tokens y costo (se registra dentro del wrapper del punto 0)
- Por cada llamada al modelo: tokens de entrada, tokens de salida, y costo estimado según
  el pricing del proveedor del servicio (AWS Bedrock, GCP Vertex, Azure AI Foundry, etc.).
- En AWS Bedrock, estos datos vienen en la respuesta del `invoke` (`usage.inputTokens`,
  `usage.outputTokens`); calcula el costo con el precio por 1K/1M tokens del modelo usado
  (recuerda: los modelos Claude más nuevos en Bedrock requieren Cross-Region Inference
  Profile con prefijo `us.`, lo cual no cambia el pricing pero sí el identificador de modelo
  a loggear).
- Usa logging estructurado (CloudWatch structured logging si es AWS) para que estos campos
  sean consultables después, no solo texto libre.
- Ubicación por defecto: `logs/tokens-costo/YYYY-MM-DD_HHMMSS.jsonl`, un archivo por
  ejecución (timestamp de inicio de la corrida; un registro estructurado por llamada al
  modelo dentro del archivo), salvo que el proyecto ya tenga una convención de logging
  distinta (revísala primero).

## 3. Uso de agentes (se registra dentro del wrapper del punto 0)
- Si hay más de un agente/subagente en la orquestación, registra tiempo de ejecución por
  agente y calcula el % de tiempo total y % de invocaciones que le corresponde a cada uno.
- Ubicación por defecto: `logs/uso-agentes/YYYY-MM-DD_HHMMSS.jsonl`, un archivo por
  ejecución (timestamp de inicio de la corrida).

## 4. Tiempos de ejecución desglosados (se registra dentro del wrapper del punto 0)
- Para cada ejecución end-to-end, registra la duración de cada etapa (p. ej.: parsing de
  la solicitud, llamada a cada agente, cada query a Redshift, generación de la respuesta
  final) para poder identificar cuellos de botella.
- Ubicación por defecto: `logs/tiempos-ejecucion/YYYY-MM-DD_HHMMSS.jsonl`, un archivo por
  ejecución (timestamp de inicio de la corrida).

## 5. Reporte periódico de estatus (cron)
Todo proyecto con agentes que aplique debe tener un job programado (cron / EventBridge
Scheduler / equivalente) que reporte cada N minutos:
- Tokens de entrada/salida acumulados y costo asociado en el periodo.
- Detalle de lo ejecutado, lo que se está ejecutando actualmente y lo que falta por
  ejecutar (si el proyecto procesa un batch/cola).
- Tiempo promedio estimado (ETA) de ejecución restante.
- Número de queries SQL ejecutados en el periodo (si aplica).

**Antes de implementar el cron, pregunta el intervalo deseado; si el usuario no
especifica uno, usa 20 minutos por defecto.**

Guarda cada reporte generado en `logs/reportes-cron/YYYY-MM-DD_HHMMSS.jsonl` (un archivo
por corrida del cron, con el timestamp exacto de esa corrida), así el historial de
reportes queda trazable, no colisiona entre corridas del mismo día, y no solo vive en el
destino de notificación (Slack, email, etc.).

Este cron es parte del código del proyecto (p. ej. una Lambda con EventBridge Scheduler
en AWS), no una automatización de Claude Code.
