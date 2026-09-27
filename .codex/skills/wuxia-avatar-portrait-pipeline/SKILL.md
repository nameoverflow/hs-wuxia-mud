---
name: wuxia-avatar-portrait-pipeline
description: Generate, compare, or QA this wuxia MUD's character portraits in the locked v4 parchment style, including derived iterations and thumbnail exports.
---

# Wuxia Avatar Portrait Pipeline

## Source and style constraints

- The locked source is `docs/assets/character-portraits/wuxia-avatar-beauty-scale-v4/originals/`.
  Do not crop, redraw, downsample, overwrite or reinterpret these originals unless
  the user explicitly provides a replacement for that exact score.
- Start further style or identity changes as a new iteration, normally v5+.
  Full-size originals are canonical; thumbnails are derived browsing aids.
- Before generating or accepting portraits, read
  [v4-style-lock.md](references/v4-style-lock.md) for the style, composition,
  angle templates, score semantics and visual rejection gates. Inspect relevant
  v4 originals with `view_image`; a full-set comparison needs all six anchors,
  individually or in a contact sheet.
- New portraits must retain square head-and-collar framing, parchment background,
  face/angle diversity and the reference's upper-body limits. Do not conceal a
  generation failure by cropping, blurring, painting or downsampling the original.

## Choose the requested work

For generation, follow the system `imagegen` skill and use built-in `image_gen`.
Use one call per distinct character or score, applying the reference template's
`STRICT COMPOSITION` and the selected `STRICT ANGLE` block. Generate a full square
original, not a thumbnail. Inspect results and correct failed style or composition
through regeneration. For QA-only tasks, inspect existing sources without
regenerating or replacing them.

For thumbnail/UI preview requests, derive the requested `96/` or `128/` exports
from canonical originals. Repair a bad export by re-exporting, leaving its source
unchanged.

## QA and artifact locations

Keep prompts, acceptance notes, manifests and reusable contact sheets in
`harness/portrait-generation/jobs/<job>/`. Use `harness/tmp/portrait-qa/` for
throwaway QA. From the repository root:

```bash
.codex/skills/wuxia-avatar-portrait-pipeline/scripts/qa-portrait-set.sh \
  --out harness/portrait-generation/jobs/<job>/qa \
  docs/assets/character-portraits/<iteration>/originals/*.png
```

The script uses `ffprobe` for dimensions and `ffmpeg` for a contact sheet. Its
size default is `1254x1254`; use `--expected-size any` for a review without that
fixed-size requirement. `--cols` and `--tile` control contact-sheet layout.
Open `contact-sheet.png` with `view_image` and inspect questionable images
individually against the reference checklist. Dimension checks alone do not
establish visual acceptance.

## Completion

Save accepted full-size originals under the iteration's `originals/` directory
in `docs/assets/character-portraits/` using stable filenames. Do not leave final
assets only in the generation tool's output directory. When delivering a new
accepted iteration, update `docs/character-portrait-style.md` with project-relative
canonical paths and freeze state; rejected candidates belong in job notes.
Do not put transient generation paths in project documentation.

Complete the requested generation/export and visual QA, correcting in-scope
failures before delivery. Report canonical paths, prompt/QA evidence and any
rejected or unresolved scores. Preserve the locked originals throughout.
