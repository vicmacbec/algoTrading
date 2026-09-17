#!/bin/bash
# Se dispara después de cualquier comando Bash. Si fue un `git commit`, reporta stats.
INPUT=$(cat)
CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if echo "$CMD" | grep -q "git commit"; then
  echo "Estadísticas del último commit:"
  git diff --shortstat HEAD~1 HEAD 2>/dev/null
  echo "Archivos:"
  git diff --stat HEAD~1 HEAD 2>/dev/null
fi

exit 0
