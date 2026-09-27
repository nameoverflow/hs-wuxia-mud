---
name: combat-keyframe-generator
description: Generate, edit, or normalize battle actor keyframes for this wuxia MUD. Use for sprite artwork and pose sets, not timeline-only code changes.
---

# Combat Keyframe Generator

Produce a small set of identity-consistent actor poses. Use the built-in
`image_gen` tool and the system `imagegen` skill for artwork; mechanical
normalization must not redraw poses or style.

## Essential constraints

- Preserve the accepted v13 actor family. Before generating, read
  [character-identity.md](references/character-identity.md) and inspect the relevant
  male/female anchor under `docs/assets/battle-animation/` with `view_image`.
  Keep unrelated old/rejected art out of the generation reference set; the
  identity reference is the anchor, with a pose-only draft added for restyling.
- Keep full-body readability, balanced arms, the same body scale and foot baseline,
  no clothing or facial detail, and the established silhouette gender markers.
- Actor frames exclude baked-in trails, glow and other VFX unless requested.
- Final actor frames are `256x192` RGBA PNGs, one pose per file, facing right by
  default. Read [frame-format.md](references/frame-format.md) when generating or
  accepting frames. Never fit each pose independently to the same bbox height.
- Locked anchors are visual constraints, not loose inspiration. Preserve existing
  identity across skills; introduce a new family only when the task calls for it.

## Choose the relevant workflow

| Task | Guidance |
| --- | --- |
| New poses or action prompt | [generation.md](references/generation.md): workflow, slot semantics and prompt template; consult `docs/combat-animation-design-guide.md` for motion intent |
| Good pose with style drift | [generation.md](references/generation.md): anchor restyle pass; anchor supplies identity, draft supplies pose only |
| Slice, key, resize or align existing generated art | [normalization.md](references/normalization.md) and the format requirements; no new generation unless the source fails acceptance |
| Wire accepted frames into playback | `docs/battle-animation.md`; use `animation-visual-qa` when motion verification is part of the task |

## Artifacts and completion

Keep prompts, reusable candidates and acceptance notes under
`harness/animation-qa/runs/<job>/`; disposable previews and intermediate files go
under `harness/tmp/keyframe-jobs/<job>/`. Copy accepted final frames into
`client/src/assets/battle/actors/`, not only the tool's generated-images folder.

For an asset-only task, complete generation, action/style inspection and format
normalization. For an integration task, also connect the catalog, run affected
checks and inspect the running animation. Fix in-scope failures before delivery;
if generation remains unable to meet a hard constraint, report the limitation
rather than silently substituting hand-drawn artwork. Report accepted paths,
rejected or unresolved slots, and relevant prompt/QA evidence.
