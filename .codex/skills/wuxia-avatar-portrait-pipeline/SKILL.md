---
name: wuxia-avatar-portrait-pipeline
description: Use when Codex needs to generate, freeze, compare, or QA game character avatar portraits for this wuxia MUD in the locked v4 hand-drawn parchment style. Applies to beauty-score portrait sets, v5+ iterations derived from v4 anchors, prompt writing, built-in image_gen calls, saving generated originals under docs/assets, thumbnail/contact-sheet exports, and visual QA for square ratio, rough brushwork, strict head-and-collar framing, upper-body proportion limits, angle diversity, face diversity, crop safety, and locked source identity.
---

# Wuxia Avatar Portrait Pipeline

Use this skill for Jianghu character portrait assets. The default generation path is Codex's built-in `image_gen` tool, followed by local save, project-local QA, and documentation updates.

## Core Rules

- Treat `docs/assets/character-portraits/wuxia-avatar-beauty-scale-v4/originals/` as the locked v4 source of truth.
- Do not crop, redraw, downsample, overwrite, or reinterpret locked v4 originals unless the user explicitly provides a replacement for that exact score.
- Start further style or identity changes as a new iteration, normally `wuxia-avatar-beauty-scale-v5`.
- Keep full-size generated originals as canonical assets. Thumbnails are derived browsing aids only.
- Enforce the head-and-collar composition gate for new portraits: face plus hair should fill most of the square, while clothing stays as a narrow collar/shoulder hint.
- Reject new generated portraits that read as half-body, chest-heavy bust, broad-shoulder portrait, or same-angle 45-degree repetition unless the user explicitly chooses that failure mode.
- Never leave project-bound final images only under `$CODEX_HOME/generated_images`.
- Never write `$CODEX_HOME/generated_images`, `.codex/generated_images`, `/tmp`, or other transient generation paths into project docs.

## Required Reference

Before generating or accepting portraits, read `references/v4-style-lock.md`. It contains the fixed visual spec, canonical paths, prompt template, beauty-score semantics, and QA checklist.

Inspect the relevant v4 original anchors with `view_image`. For a full set, inspect all six locked originals or create a contact sheet with the QA script below.

## Workflow

1. Define the task:
   - requested score or character role
   - whether this is v4 replacement by explicit user instruction, or a new v5+ iteration
   - final workspace destination under `docs/assets/character-portraits/`
   - expected output set and filenames
2. Prepare a job folder:

```bash
mkdir -p reports/portrait-jobs/<slug>
```

Save prompts, accepted/rejected notes, and QA output there. Do not use the job folder as the final asset location.

3. Generate with built-in `image_gen`:
   - If actually calling `image_gen`, first follow the system `imagegen` skill instructions.
   - Use one `image_gen` call per distinct character or score.
   - Use the prompt template from `references/v4-style-lock.md`.
   - Include the `STRICT COMPOSITION` and `STRICT ANGLE` blocks from the reference in every prompt.
   - Ask for a full square original portrait, not a cropped thumbnail.
   - Keep the background as aged parchment; do not ask for transparency.

4. Save project assets:
   - Copy the accepted generated PNG from the default generated-images location into the workspace.
   - Use stable filenames such as `beauty-score-04-source.png`.
   - Store canonical source images under an `originals/` directory.
   - Only create `96/` or `128/` exports when the user asks for thumbnails or UI previews. Do not treat them as source assets.

5. QA the output:

```bash
.codex/skills/wuxia-avatar-portrait-pipeline/scripts/qa-portrait-set.sh \
  --out reports/portrait-jobs/<slug>/qa \
  docs/assets/character-portraits/<iteration>/originals/*.png
```

Open the generated `contact-sheet.png` with `view_image`, then inspect any questionable image individually. Apply the QA checklist in `references/v4-style-lock.md`.

Default reject gates for new v5+ portraits:

- face plus hair does not visually occupy about 84-90% of the square
- clothing/upper body occupies more than about the bottom 12% in normal views, or more than 18% in an explicit over-shoulder/back-turn view
- visible chest, waist, arm, hand, large sleeve, or broad torso mass
- two requested portraits share the same 45-degree half-profile angle
- a prompt asked for front view, reverse direction, or top-down view but the output collapses back to the usual same-side 45-degree view

6. Iterate narrowly:
   - If a portrait fails style QA, regenerate with one targeted prompt correction.
   - Do not locally crop, blur, paint over, or downsample an original to hide a generation failure.
   - If only a thumbnail export is bad, regenerate the thumbnail from the canonical original without changing the original.

7. Update documentation:
   - Update `docs/character-portrait-style.md` with the iteration name, canonical original paths, and freeze state.
   - Use project-relative paths in Markdown.
   - Mention rejected variants only in the job notes, not as canonical assets.

## QA Script

`scripts/qa-portrait-set.sh` validates dimensions with `ffprobe`, writes `manifest.tsv`, and creates a contact sheet with `ffmpeg`.

Examples:

```bash
.codex/skills/wuxia-avatar-portrait-pipeline/scripts/qa-portrait-set.sh \
  --out reports/portrait-jobs/v4-lock/qa \
  docs/assets/character-portraits/wuxia-avatar-beauty-scale-v4/originals/*.png
```

```bash
.codex/skills/wuxia-avatar-portrait-pipeline/scripts/qa-portrait-set.sh \
  --expected-size any \
  --cols 4 \
  --tile 180 \
  --out reports/portrait-jobs/freeform-review/qa \
  path/to/images/*.png
```

## Deliverables

For a completed portrait task, report:

- canonical workspace asset paths
- prompt summary or prompt file path
- QA artifacts path
- accepted and rejected images
- any remaining visual risk or scores that still need user confirmation
