#!/bin/bash
# Se dispara ANTES de un `git commit`. Bloquea si hay llamadas crudas al modelo
# fuera de src/services/observability.py (proyectos con agentes).
INPUT=$(cat)
CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

echo "$CMD" | grep -q "git commit" || exit 0
[ -d "src/agentes" ] || exit 0

# Patrón afinado a las llamadas reales del SDK usado (ajusta si tu proyecto usa otro):
# - boto3 bedrock-runtime: client.invoke_model( / client.converse(
# - Strands Agents: strands.Agent( / Agent(...).invoke(
# - bedrock-agentcore-starter-toolkit: BedrockAgentCoreApp( / .invoke(
PATTERN='bedrock_runtime\.(invoke_model|converse)\(|strands\.Agent\(|BedrockAgentCoreApp\('

# Excluye archivos de test: ahí SÍ es legítimo instanciar el cliente crudo para probarlo
OFFENDERS=$(git diff --cached --name-only -- 'src/agentes/*' 'src/tools/*' \
  | grep -vE '(^|/)(test_|.*_test\.py$|conftest\.py$)' \
  | xargs -r grep -lE "$PATTERN" 2>/dev/null)

if [ -n "$OFFENDERS" ]; then
  echo "Bloqueado: llamada al modelo fuera de src/services/observability.py en:"
  echo "$OFFENDERS" >&2
  echo "Usa el wrapper invoke_agent_con_trazabilidad(...) en su lugar." >&2
  exit 2
fi

exit 0
