---
name: animation-visual-qa
description: Use when Codex needs to QA, critique, or tune an animation effect against a natural-language expectation in this project. Records or collects an animation run, converts video into dense storyboard/contact-sheet images, uses Codex visual inspection to make subjective judgments about motion clarity, timing, impact, style fit, and visible problems, then iterates on animation code or assets.
---

# Animation Visual QA

Use this skill when the user asks Codex to judge whether an animation "looks right", matches a written description, has enough impact, feels smooth, or has visual problems. The default reviewer is Codex's built-in visual ability: create local storyboard images, inspect them with `view_image`, and write a subjective review. Do not default to an external API unless the user asks for unattended or CI-style review.

## Workflow

1. Get the expectation in the user's words. If it is missing, ask for one concise description before judging subjective fit.
2. Reach the pre-trigger state normally: run the app, use browser automation, seed fixtures, or use a dedicated animation lab if one exists.
3. Record only the relevant window/flow. Start recording immediately before triggering the animation, then trigger the animation or full interaction.
4. Convert the recording into storyboard images:

```bash
.codex/skills/animation-visual-qa/scripts/storyboard-from-video.sh \
  reports/animation-qa/example/recording.webm \
  reports/animation-qa/example/storyboards \
  3 16 8 240
```

5. Inspect every generated storyboard with `view_image`. If there are multiple segments, review them in order.
6. Judge the animation subjectively against the expectation:
   - action semantics: does it read as the described action?
   - timing and rhythm: does it feel too slow, rushed, floaty, or late?
   - impact: are hit, parry, dodge, trail, flash, and damage feedback legible?
   - continuity: do poses and effects transition naturally across the storyboard?
   - composition: are actors, effects, text, and UI visible without awkward overlap?
   - style fit: does it match this project's wuxia silhouette direction rather than generic UI motion?
7. Produce a concrete review:
   - `Pass`, `Borderline`, or `Fail`
   - score from 0 to 100
   - 3-7 findings, each with segment/frame evidence when possible
   - implementation suggestions such as duration, easing, delay, opacity, pose, trail, camera, or z-index changes
8. If the user asked Codex to tune the animation, implement the highest-impact fixes, rerun the recording/storyboard step, and compare before/after.

## Recording Options

Prefer the least invasive recording method that produces a local video:

- If a browser automation script already exists, add Playwright video recording around the animation trigger.
- If no recorder exists, adapt `scripts/record-playwright.mjs`. It is intentionally generic and supports URL, trigger selector, duration, and output directory.
- If the animation is hard to trigger deterministically, first build a tiny project-local animation lab or fixture page that can play the target animation from a fixed state.
- If video recording is unavailable, use high-frequency screenshots as a fallback and tile them into storyboard images. State that this is a weaker review.

## Evidence Rules

- Use dense storyboards, not a few screenshots. A normal 3-second segment should use about 48 frames (`16 fps`, `8 x 6` tile).
- Keep raw `recording.webm` and generated `storyboard-*.png` under a task folder such as `reports/animation-qa/<case>/`.
- Review the whole segment before editing. Do not judge motion from the first visible frame only.
- Treat the model's visual review as subjective evidence. It is allowed to say "looks weak", "reads as a slide", or "impact is late", but should cite the visible frames or segment positions that caused the judgment.
- Do not claim exact frame-rate smoothness unless measured separately. This skill is for visual and subjective animation QA.

## Scripts

- `scripts/storyboard-from-video.sh <video> <out-dir> [segment_seconds=3] [fps=16] [cols=8] [tile_width=240]`
  - Uses `ffmpeg` to split the video into time windows and generate dense storyboard PNGs.
- `scripts/record-playwright.mjs`
  - Optional generic browser recorder. Requires Playwright to be available in the invoking project or environment.
- `scripts/make-review-prompt.mjs`
  - Writes a Markdown review prompt that lists the expectation, recording, and storyboard files for Codex visual inspection.

## Review Shape

Use this structure in the final review or working note:

```text
Verdict: Fail / Borderline / Pass
Score: 0-100

Expectation:
<user description>

Findings:
- [major] Segment 001, middle rows: ...
- [minor] Segment 001, last row: ...

Recommended changes:
- ...

Artifacts:
- recording: ...
- storyboards: ...
```
