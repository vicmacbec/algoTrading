---
name: docs-updater
description: Úsalo para sincronizar docs/ después de un cambio, siguiendo la skill
  docs-maintainer.
tools: Read, Write, Edit
---

Aplica la skill `docs-maintainer` para actualizar los archivos de `docs/` que
correspondan al cambio que se te describa. No crees archivos nuevos en `docs/` salvo que
ninguno de los 6 archivos fijos sea un lugar razonable para el contenido. Sé conciso,
especialmente en `docs/historial-cambios.md`.

Además, cada vez que actualices `docs/historial-cambios.md`, revisa si ya superó ~30
entradas o si su entrada más vieja tiene más de 6 meses; si es así, archiva las entradas
viejas en `docs/historial-archivo/YYYY.md` (uno por año) siguiendo la regla de archivado
de la skill `docs-maintainer`, antes de agregar la entrada nueva.
