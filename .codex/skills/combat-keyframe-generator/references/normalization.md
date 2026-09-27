# Keyframe Normalization

Use for mechanical processing of generated artwork. Commands run from the repository root. Keep reusable sources and manifests in the job folder; disposable previews belong in `harness/tmp/`.

## Deterministic Normalization

Use the bundled normalizer for generated sheets:

```bash
.codex/skills/combat-keyframe-generator/scripts/normalize_keyframes.py \
  --source harness/animation-qa/runs/test-sword-male/generated/source-sheet.png \
  --out harness/animation-qa/runs/test-sword-male/accepted \
  --project-out client/src/assets/battle/actors/test-sword/male \
  --slots idle,slash,stab,uppercut,guard \
  --cols 5 \
  --reference-slot idle \
  --preview harness/tmp/keyframe-jobs/test-sword-male/preview.png \
  --manifest harness/animation-qa/runs/test-sword-male/manifest.json
```

The normalizer intentionally fails when the actor touches a source cell edge. Treat that as a bad generation and regenerate with more padding; do not crop through the edge or use an overlapped neighbor cell as a silent fix.

The normalizer defaults to `--mode components`: remove chroma key, group source components into the requested number of poses by row/column spacing, then normalize those complete poses. This is safer for AI-generated sheets where invisible cell boundaries are not exact. Use `--mode grid` only for strict deterministic sheets.

The normalizer uses one shared scale for the whole sheet, derived from the reference slot and constrained by the largest source pose. This preserves visual size differences such as crouches, kicks, lunges, and recoils while fixing the foot baseline. It also drops tiny stray fragments after grouping.

## Chroma And Transparency

The built-in `image_gen` path does not expose a native transparent-output setting. Ask for a flat chroma-key background, usually `#00ff00`, then remove it locally with the installed helper from the system `imagegen` skill if a transparent PNG is needed:

```bash
python "${CODEX_HOME:-$HOME/.codex}/skills/.system/imagegen/scripts/remove_chroma_key.py" \
  --input <generated-source.png> \
  --out <final-rgba.png> \
  --auto-key border \
  --soft-matte \
  --despill
```

If chroma removal fails because the subject uses the key color, regenerate with a different flat key such as `#ff00ff`.
