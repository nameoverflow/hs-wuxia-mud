# Raster keyframes v1

This run replaces the segmented-v12 skeletal actor with generated discrete PNG keyframes while preserving action timing, impact cues, stage motion, and VFX.

Asset groups:

- `fist-body`: ten shared empty-hand body poses.
- `sword-body`: sixteen shared jian body poses.
- female profiles reuse the exact same body PNG and add a pose-matched ponytail overlay.

Final runtime assets are mirrored under `client/src/assets/battle/actors/raster-v1/` after normalization and visual QA.

Accepted generated sources:

- `fist-body/generated/source-sheet-v2.png`
- `fist-hair/generated/source-sheet.png`
- `sword-body/generated/source-sheet-v3.png`
- `sword-hair/generated/source-sheet.png`

See `review.md` for the generation iterations, semantic frame mapping, and QA verdict.
