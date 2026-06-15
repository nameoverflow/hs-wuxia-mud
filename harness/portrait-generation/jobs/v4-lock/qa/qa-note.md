# v4 Portrait Lock QA

Verdict: Pass

Scope: locked v4 originals for beauty scores 0, 2, 4, 6, 8, and 10.

Evidence:

- `manifest.tsv` reports all six canonical originals as `1254x1254`.
- `contact-sheet.png` shows the expected order: 0, 2, 4, 6, 8, 10.
- Score 4 uses the user-confirmed freckled source image.
- Scores 6, 8, and 10 preserve the user-confirmed original identities.

Notes:

- The full-size originals in `docs/assets/character-portraits/wuxia-avatar-beauty-scale-v4/originals/` are canonical.
- Existing `96/` and `128/` files remain thumbnail exports only.
- Any future visual changes should start a v5 iteration.
