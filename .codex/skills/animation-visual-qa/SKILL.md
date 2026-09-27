---
name: animation-visual-qa
description: Review or tune this project's animation appearance and motion using recorded storyboards. Use for visual judgments, not logic-only animation tests.
---

# Animation Visual QA

Judge the visible action against the requested effect using local recordings and
Codex visual inspection. Use an external review API only if requested.

## Expectation and evidence

Infer the intended result from the user's description, the action definition and
`docs/combat-animation-design-guide.md`. State assumptions when relevant. Ask only
when an unresolved ambiguity would materially change the judgment.

Capture the relevant interaction from a reproducible pre-trigger state. Prefer
the existing battle lab and fixtures before adding a new recorder or fixture page.
Review the whole relevant sequence, not just its opening frame.

## Capture and inspect

For battle animation, start the client with `npm --prefix client run dev -- --port 8080`.
The existing recorder captures the lab demo; for a specific action, use the lab
controls or a focused recording around that action. From the repository root:

```bash
BATTLE_QA_OUT="$PWD/harness/tmp/animation-qa/<case>" npm --prefix client run record:battle
.codex/skills/animation-visual-qa/scripts/storyboard-from-video.sh \
  harness/tmp/animation-qa/<case>/recording.webm \
  harness/tmp/animation-qa/<case>/storyboards \
  3 16 8 240
```

`BATTLE_QA_URL` selects another running client URL. For non-battle flows,
`scripts/record-playwright.mjs` supports URL, trigger selector, duration and output
directory. If recording is unavailable, tiled high-frequency screenshots are a
weaker fallback; disclose that limitation.

Inspect the storyboards with `view_image` in sequence. About 48 samples per
3-second segment is a useful default; increase sampling around brief contact or
hit-stop events if the sequence does not resolve them. Judge action semantics,
rhythm, impact, continuity, composition and style fit. Storyboards alone do not
establish exact frame-rate smoothness; measure that separately when requested.

## Review and completion

Report `Pass`, `Borderline` or `Fail`, the expectation, and actual findings with
segment/time/frame evidence. Include only findings supported by the capture.
Use a numeric score only when a comparison needs it, with an explicit rubric.
`scripts/make-review-prompt.mjs` can assemble an evidence-linked review prompt.

For a review request, deliver the findings and suggested changes. For a tuning
request, implement in-scope fixes, run checks relevant to the changed behavior,
record again and compare the result. Finish when the requested effect meets the
stated criteria, or report the concrete remaining blocker. A passing code check
does not substitute for inspecting the revised animation.

Keep raw recordings, storyboards and other regenerable captures under
`harness/tmp/animation-qa/<case>/`. Save reusable expectations, verdicts and decision
notes under `harness/animation-qa/runs/<case>/`; keep reusable reference clips under
`harness/animation-qa/references/`.
