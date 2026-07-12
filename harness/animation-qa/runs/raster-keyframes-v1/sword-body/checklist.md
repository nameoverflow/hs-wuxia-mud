# Job Checklist

1. Read `.codex/skills/combat-keyframe-generator/references/frame-format.md`.
2. Inspect `docs/assets/battle-animation/actor-style-reference-male.png` or `docs/assets/battle-animation/actor-style-reference-female.png`; use the correct image as the visual reference for body proportions and gender marker.
3. Keep weapon family and visual identity separated: do not mix sword, fist, male, female, or common reactions in one runtime pool.
4. Generate a job anchor with built-in image_gen only if a new actor family is needed.
5. Save selected source images under `harness/animation-qa/runs/raster-keyframes-v1/sword-body/generated/`.
6. Inspect with view_image.
7. Gate action and style separately. If action passes but style fails, use the v13 anchor as identity/style reference and the draft as pose-only reference, then save the corrected frame under `harness/animation-qa/runs/raster-keyframes-v1/sword-body/restyled/`.
8. Reject and regenerate any source where the actor or weapon touches the source sheet edge, or a source cell edge when using grid mode.
9. Normalize final accepted frames to `256x192` RGBA PNG with `scripts/normalize_keyframes.py` or the same shared-scale algorithm. Never resize each frame independently to its own bbox height.
10. Save accepted frames under `harness/animation-qa/runs/raster-keyframes-v1/sword-body/accepted/`.
11. Copy final project assets under `client/src/assets/battle/actors/...`.
12. If animation motion matters, wire frames and run `$animation-visual-qa`.
