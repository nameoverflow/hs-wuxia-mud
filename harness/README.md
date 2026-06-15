# Agent Harness Artifacts

This directory stores agent-generated material that is useful to keep around for repeatable QA, future prompts, or design decisions.

## Layout

- `animation-qa/runs/`: review prompts, QA verdicts, bounding boxes, and other lightweight records from local animation checks.
- `animation-qa/references/`: reusable reference clips and analysis notes for animation timing or visual style.
- `portrait-generation/jobs/`: prompt histories, QA manifests, notes, and contact sheets for portrait-generation jobs.
- `portrait-generation/candidates/`: reusable generated candidates that are not final in-game or documentation assets yet.
- `tmp/`: ignored runtime scratch space for recordings, storyboards, crop tests, generated lists, markers, and other files that can be regenerated.

## What To Commit

Commit files that preserve a decision, a repeatable prompt, a source reference, or a reusable generated candidate. Put pure execution byproducts under `harness/tmp/` so they stay local.
