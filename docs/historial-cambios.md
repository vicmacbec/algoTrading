# Historial de cambios

Changelog corto: una entrada por cambio significativo, con el hash del commit. El detalle de
implementación vive en el commit, no aquí.

## [aad8789] 2026-09-16
Se sustituyó `docs/estructura-del-proyecto.md` por los 6 archivos fijos de la convención —
la documentación estaba en un solo archivo que mezclaba estructura, arquitectura, reglas de
negocio y operación, y no había dónde registrar pendientes ni cambios.

## [cdbe1b7] 2026-09-16
Se renombró `Scripts/` a `src/` y se actualizaron las rutas del wrapper `.sh`, del script
productivo, del `.gitignore` y del README — para adoptar la convención de carpetas de la skill
`project-structure`. El despliegue de la EC2 y su crontab quedan pendientes.

## [c65193a] 2026-09-16
Se agregaron `logs/`, `.claude/settings.local.json` y `CLAUDE.local.md` al `.gitignore` —
son artefactos de runtime y configuración personal que no deben compartirse en el repo.

## [c8a3390] 2026-09-16
Se agregaron `CLAUDE.md` y `.claude/` con skills, comando, subagentes y hooks — para fijar el
flujo de trabajo con Claude Code (plan previo, alcance acotado, docs sincronizados, stats por
commit y confirmación antes de `git push`).
