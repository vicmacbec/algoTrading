---
description: Flujo completo para implementar un cambio de código, de principio a fin.
---

Vas a implementar el siguiente cambio: $ARGUMENTS

Si no se especificó ningún cambio arriba (el mensaje termina en "Vas a implementar el
siguiente cambio:" sin nada después), pregunta primero qué cambio se quiere hacer antes
de continuar — no sigas los pasos de abajo con un alcance vacío.

**Carril rápido:** si el cambio es de una sola línea o es solo texto (typo, copy,
comentario, valor de config trivial), salta el paso 3 (Plan) y la entrada de changelog
del paso 6 — solo actualiza el archivo de `docs/` directamente afectado, si alguno lo
está. Ante la duda, sigue el flujo completo.

Sigue este flujo, en orden, sin saltarte pasos (salvo lo que indique el carril rápido):

1. **Investigación.** Lee el código relevante. De `docs/`, lee siempre
   `docs/estructura-y-flujo.md`, `docs/arquitectura.md` y `docs/pendientes.md` (los tres
   son cortos por diseño, así que su costo en contexto es bajo, y `pendientes.md` te dice
   si el cambio pedido ya estaba anticipado). Lee además `docs/reglas-negocio.md` si el
   cambio toca lógica de negocio, métricas o fórmulas, y `docs/operacion.md` si toca
   despliegue/infraestructura — no leas ambos por rutina si el cambio no los involucra.
   No leas `docs/historial-cambios.md` completo salvo que necesites entender el porqué de
   un cambio pasado específico; en ese caso, busca la entrada puntual en vez de leer todo
   el archivo. Entiende cómo encaja el cambio pedido en lo que ya existe.

2. **Preguntas.** Si algo no está claro, o detectas otra área de oportunidad relacionada,
   pregúntalo ahora — no avances con supuestos. Si consideras que alguna parte de lo
   pedido no es adecuada, dilo y explica por qué, antes de planear.

3. **Plan.** *(Se omite en carril rápido.)* Escribe un plan breve de qué archivos vas a
   tocar/crear y por qué. Espera confirmación si el cambio es grande o ambiguo.

4. **Implementación.** Aplica solo los cambios del plan confirmado (o, en carril rápido,
   el cambio pedido tal cual). No toques nada fuera de alcance.

5. **Pruebas.** Crea o actualiza las pruebas unitarias necesarias en `test/`. Corre la
   suite completa y confirma que nada existente se rompió y que el propósito del proyecto
   (lo existente + lo nuevo) se sigue cumpliendo.

6. **Documentación.** Actualiza los archivos de `docs/` que correspondan según la skill
   `docs-maintainer`, y actualiza `docs/pendientes.md` si aplica. *(La entrada en
   `docs/historial-cambios.md` se omite en carril rápido; fuera de ese caso, agrégala una
   vez tengas el hash del commit.)*

7. **Commit.** Antes de commitear:
   - Pregunta en qué rama debe ir el commit **solo si** no se indicó una rama en la
     petición y no estás ya en una rama de trabajo distinta de `main`/`master` — si ya
     hay una rama de feature activa, usa esa sin preguntar.
   - Pregunta si corresponde hacer merge a otra rama solo cuando el cambio cierre una
     unidad de trabajo completa, no en cada commit individual.
   - Haz commits atómicos y descriptivos (uno por unidad lógica de cambio, no todo en uno
     solo, para facilitar un revert parcial si hace falta).
   - Después de cada commit, reporta cuántos archivos fueron tocados y cuántas líneas se
     agregaron/quitaron (esto se genera automáticamente vía hook; solo repórtalo al
     usuario).
