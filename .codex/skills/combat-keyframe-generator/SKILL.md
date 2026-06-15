---
name: combat-keyframe-generator
description: Use when Codex needs to generate, refine, or format AI-created 2D battle animation keyframes for this wuxia MUD project using the built-in image_gen tool. Applies to requests for natural combat poses, actor keyframes, hit/dodge/parry reaction frames, silhouette sprite assets, animation pose sets, frame prompt generation, frame-format enforcement, or turning a user action description into project-ready 256x192 PNG keyframes guided by docs/combat-animation-design-guide.md.
---

# Combat Keyframe Generator

Use this skill to turn a user's combat-action description into a small, reliable set of project-ready 2D keyframes. The default generation path is Codex's built-in `image_gen` tool, not an OpenAI API script. Use this together with `$animation-visual-qa` when the generated frames are wired into animation and need subjective motion review.

## Core Rule

Generate key poses, not finished motion. Natural animation comes from pose clarity, non-linear movement, short-lived effects, target feedback, and readable pauses. Before generating, read or skim `docs/combat-animation-design-guide.md`; use `docs/battle-animation.md` only when integrating the frames into catalog/timeline/protocol work.

Keep actor frames extremely simple and reusable. Skill-specific personality should mostly come from pose timing and separate VFX layers, not from clothing, hair detail, face, weapon glow, or painted attack trails inside the actor frame.

## Canonical Actor Anchors

The accepted v13 abstract actor anchors are part of this skill and mirrored into the project:

- Combined anchor: `docs/assets/battle-animation/actor-style-anchor-canonical.png`
- Male reference: `docs/assets/battle-animation/actor-style-reference-male.png`
- Female reference: `docs/assets/battle-animation/actor-style-reference-female.png`
- Skill asset mirrors: `.codex/skills/combat-keyframe-generator/assets/actor-style-anchor-canonical.png`, `actor-style-reference-male.png`, and `actor-style-reference-female.png`

Before generating actor frames, inspect the relevant reference image with `view_image`. Treat these anchors as visual constraints, not loose inspiration.

Keep the image-generation context clean. Before calling `image_gen`, do not inspect or display old actor sprites, rejected generations, previous detailed concept art, or runtime screenshots in the same generation flow unless the user explicitly asks to use one as an edit target. Do not mention "old sprites shown earlier" or other dirty references inside the generation prompt; discuss rejected references only in QA notes after generation. The intended visible reference for baseline actors is the v13 anchor only.

Male and female actors share the same abstract little-person family: same scale, modest softened head, torso mass, limb thickness, foot baseline, no face, and no clothing. The gender difference must stay silhouette-level:

- Male: no hair; slightly squarer, wider, sturdier baseline guard is acceptable.
- Female: same body family, plus one high-tied long ponytail reaching below the waist line toward the upper hip; stance may be slightly lighter, narrower, and more upright.

Do not use breasts, waist/hip detail, eyelashes, skirt, robe, jewelry, face, hair strands, or costume markers to signal gender.

Both profiles must preserve v13's arm balance: the raised front arm is not oversized, and the rear waist-side arm reads as a complete compact bent arm, not a tiny notch or missing limb.

## Workflow

1. Parse the user's request into an animation intent:
   - action family: thrust, slash, uppercut, palm, kick, projectile, dodge, parry, hurt, idle, or custom
   - actor type and weapon
   - identity profile: `male`, `female`, `neutral`, or an established project character card
   - desired style and mood
   - needed frame slots
   - final destination under `client/src/assets/battle/actors/`
2. Create a job folder and prompt pack:

```bash
.codex/skills/combat-keyframe-generator/scripts/prepare_keyframe_job.py \
  --out reports/keyframe-jobs/cold-rain-thrust \
  --name cold-rain-thrust \
  --action "a short cold-rain sword thrust, restrained but sharp" \
  --actor "project baseline sword actor" \
  --identity-profile male \
  --frames idle,windup,strike,impact,recover,target_hurt
```

3. Read the generated `prompts.md`, `references/frame-format.md`, and `references/character-identity.md`.
4. Inspect `docs/assets/battle-animation/actor-style-reference-male.png` or `docs/assets/battle-animation/actor-style-reference-female.png` with `view_image` before generating. Treat the selected image as the identity reference that must be preserved by `image_gen`, including body scale, arm balance, and gender marker. Do not inspect old or rejected actor frames before generation; inspect them only after generation when comparing outputs.
5. Use built-in `image_gen` for a new job-specific anchor only when the requested actor family is not already covered by the male/female abstract references. Save or move the selected image into the job folder.
6. Generate each frame as a separate image by default. Use a sheet only when preserving one actor identity across many poses is more important than generation independence.
   - Never mix different martial-art families in one sheet. Sword, fist, reactions, male, and female variants should be separate jobs or clearly separate sheets.
   - For sheet generation, require wide invisible cells, generous gutters, and no pose crossing its cell. Long weapons must have visible padding beyond the tip.
7. Inspect every generated frame with `view_image` and run the two-part gate:
   - Action gate: pose slot, weapon direction, off-hand role, and full-body readability.
   - Style gate: v13 body family, no clothing form, correct head shape, gender marker, arm balance, scale, and simplicity.
8. If action passes but style fails, run one anchor restyle pass before rejecting:
   - Display only two images before calling `image_gen`: the selected v13 anchor as the identity/style reference, and the failed generated frame as the pose/action reference.
   - Prompt `image_gen` to preserve the pose, sword direction, off-hand direction, and action line from the failed frame, while replacing the actor's body silhouette with the v13 anchor body family.
   - Treat the failed frame as pose-only. Do not preserve its head shape, clothing-like limbs, hair details, garment edges, texture, or rendering style.
   - Restyle one frame at a time. Do not restyle a full sheet when only some slots drifted.
   - If the restyled frame still has clothing, round-head drift, detail creep, or broken body readability, reject it and regenerate from a cleaner action prompt.
9. Reject or regenerate frames that violate hard format constraints:
   - more than one actor when only one was requested
   - cropped body or missing feet
   - inconsistent body shape, gender marker, weapon, scale, facing, or baseline
   - any clothing silhouette such as robe hems, sleeves, belts, boots, armor, uniforms, or layered garments
   - too much character detail, facial detail, hair strands, gradients, or painterly texture
   - attack trails, sword glow, sparks, scenery, or VFX baked into actor frames unless the user explicitly asked for a VFX reference frame
   - background scenery, text, UI, borders, grid, shadows, motion blur
   - pose does not match the slot semantics
10. Normalize accepted generated sources with `scripts/normalize_keyframes.py` or an equivalent shared-scale process. Do not fit each frame independently to its own bounding box.
11. Copy final selected assets into the project. Current actor frames use `256x192` RGBA PNGs under `client/src/assets/battle/actors/...`. Never leave project-bound final images only under `$CODEX_HOME/generated_images`.
12. If the user asks to validate motion, wire the frames into the animation, record with `$animation-visual-qa`, and judge the storyboard.

## Anchor Restyle Pass

Use this pass when the action is useful but the actor no longer matches v13. It is a style correction, not a new action generation.

Input roles:

- Identity/style reference: the relevant v13 anchor, either `actor-style-reference-male.png` or `actor-style-reference-female.png`.
- Pose/action reference: the flawed generated frame whose pose, sword direction, off-hand direction, and rough action line should be retained.

Prompt pattern:

```text
Create a corrected 2D game keyframe using two references.
Reference A is the identity/style anchor. Preserve its abstract v13 body family exactly: modest softened non-perfect-circle head, simple torso, rounded body-limb arms and legs, broad simple feet, no face, no clothing, no texture.
Reference B is pose-only. Preserve only its action: <slot>, sword direction, off-hand direction, body lean, and full-body composition.
Replace the actor in Reference B with the v13 actor from Reference A. Do not preserve Reference B's clothing-like shapes, round head drift, garment edges, sleeves, pants, cuffs, boots, hair strands, texture, glow, or old rendering style.
Keep one full-body actor on flat #00ff00 chroma-key background, facing right, full weapon visible, feet visible, no crop, no text, no grid, no shadows, no trails.
```

When this pass succeeds, save the corrected output separately, for example `restyled/<slot>.png`, and record the original draft as rejected or pose-only evidence. Do not overwrite the action draft until the corrected frame passes visual inspection.

## Deterministic Normalization

Use the bundled normalizer for generated sheets:

```bash
.codex/skills/combat-keyframe-generator/scripts/normalize_keyframes.py \
  --source reports/keyframe-jobs/test-sword-male/generated/source-sheet.png \
  --out reports/keyframe-jobs/test-sword-male/accepted \
  --project-out client/src/assets/battle/actors/test-sword/male \
  --slots idle,slash,stab,uppercut,guard \
  --cols 5 \
  --reference-slot idle \
  --preview reports/keyframe-jobs/test-sword-male/preview.png \
  --manifest reports/keyframe-jobs/test-sword-male/manifest.json
```

The normalizer intentionally fails when the actor touches a source cell edge. Treat that as a bad generation and regenerate with more padding; do not crop through the edge or use an overlapped neighbor cell as a silent fix.

The normalizer defaults to `--mode components`: remove chroma key, group source components into the requested number of poses by row/column spacing, then normalize those complete poses. This is safer for AI-generated sheets where invisible cell boundaries are not exact. Use `--mode grid` only for strict deterministic sheets.

The normalizer uses one shared scale for the whole sheet, derived from the reference slot and constrained by the largest source pose. This preserves visual size differences such as crouches, kicks, lunges, and recoils while fixing the foot baseline. It also drops tiny stray fragments after grouping.

## Frame Slot Semantics

Use the smallest set that communicates the action. Default attack set:

- `idle`: readable neutral combat stance, stable foot anchor.
- `windup`: slight reverse preparation, lowered center, shoulder/weapon preparing.
- `strike`: maximum extension or most identifiable action pose.
- `impact`: optional; use when contact needs a denser pose or weapon reach.
- `recover`: optional; often reuses idle or a softer settle pose.
- `target_hurt`: shared hit reaction; target recoils visibly but stays readable.
- `target_parry`: stable guarded reaction with weapon/arm raised.
- `target_dodge`: side-step or afterimage-friendly pose, feet still visible.

Do not generate every in-between pose by default. More frames are justified only when the silhouette meaning changes, not just to smooth interpolation.

## Prompt Rules

Use `image_gen` with a concise but strict prompt:

```text
Create exactly one 2D game animation keyframe.
Project: Wuxia text-MUD battle UI.
Frame slot: <slot>.
Action: <user action>.
Actor: <stable character card>.
Style: extremely simplified high-contrast abstract wuxia little-person silhouette sprite, side-view, facing right, readable at small size.
Reference: match the exact abstraction level and body proportions of the selected project reference image: `docs/assets/battle-animation/actor-style-reference-male.png` for male actors or `docs/assets/battle-animation/actor-style-reference-female.png` for female actors. Preserve the same modest head size, softly rounded non-perfect-circle head shape, torso style, limb thickness, low center of gravity, and no-clothing body silhouette.
Proportions: squat abstract little-person shape, about 2.5-3 heads tall. Male and female actors must have the same total body height, same head size, same torso scale, same limb thickness, and same foot baseline.
Human readability: must still read as a small person, not a broken icon or abstract logo. Keep a clear head-above-torso relationship, visible shoulder/hip mass, two readable arms, two readable legs, and planted feet.
Body silhouette: no clothing form at all. Male is a plain featureless little figure with a softened rounded head, simple torso, arms, and legs. Female uses the same body shape but adds one simple high-tied long ponytail reaching below the waist line toward the upper hip along the back, with a small tie/knot bump, a flowing S-curve, and a tapered or subtly split tip. The ponytail is the only gender marker.
Arm balance: front raised arm and rear waist-side arm must have similar simplified limb thickness and visual weight. The front fist must not become oversized. The rear arm must read as a complete compact bent arm near the waist/hip, not a tiny notch.
Canvas: 4:3 frame intended for final 256x192 RGBA PNG.
Background: perfectly flat solid #00ff00 chroma key for removal, no scenery.
Pose: <slot-specific pose>.
Hard constraints: one full-body actor only; feet visible; feet on the same horizontal baseline; no crop; no cast shadow; no motion blur; no text; no labels; no UI; no frame border; no grid; no background details; no attack trail or weapon glow baked into the actor frame; same body shape, same gender marker, same weapon state, same proportions as the selected reference; if generated as a sheet, the complete actor and weapon must sit well inside its invisible cell and must not touch or cross cell edges.
Avoid: clothes, robe, sleeve, belt, boots, shoes detail, armor, uniform, garment folds, hair strands, face, eyes, mouth, chibi face, mascot details, realistic anatomy, Japanese/Korean/Chinese costume silhouettes, kung-fu practice suit, gradients, painterly texture.
Simplicity constraints: flat 1-2 color silhouette, no facial features, no eyes, no mouth, no clothing, no hair strands, no ornate accessories, no gradients, no painterly texture, no internal line art except one or two necessary cutout gaps for pose readability.
Reject if the figure stops reading as a person, even when the scale and gender marker are correct.
```

For reaction frames, replace `Actor` with the defender description and state the result clearly: hit recoil, guarded parry, or dodge lean. For left-facing use, generate facing right first and mirror in code unless an asymmetrical asset truly needs a dedicated left-facing frame.

## Unified Actor Identity

Read `references/character-identity.md` before creating new actor frames. Default to the existing project baseline actor profiles instead of inventing a new character per skill.

- Canonical visual anchors: `docs/assets/battle-animation/actor-style-reference-male.png` and `docs/assets/battle-animation/actor-style-reference-female.png`; skill asset mirrors live under `.codex/skills/combat-keyframe-generator/assets/`.
- Male and female variants should differ only by silhouette profile: male has no hair and may be slightly squarer; female keeps the same body family with the high-tied long ponytail and may be slightly lighter/narrower. Do not add costume or anatomy detail to signal gender.
- Generate male and female variants as separate identity-consistent sheets or frame sets. Do not ask one sheet to alternate gender unless the output is only a style reference, because later slicing and catalog wiring need stable identity per set.
- Skill animations should preserve the same actor identity. A cold-rain thrust, slash, dodge, and parry should look like the same person changing pose.
- Do not encode skill identity as costume changes. Encode skill identity through pose, motion timing, and separate VFX.
- If the task needs a new actor family, create a reusable character card first, then generate action frames from that card.

## Format Requirements

Read `references/frame-format.md` before generating or accepting final frames. The short version:

- final project frame: `256x192` RGBA PNG
- one actor per frame
- extremely simplified silhouette, not detailed character art
- orthographic side view
- facing right unless explicitly requested
- full body, feet visible, no crop
- stable foot baseline across all frames
- shared scale across all frames in a sheet or action set; never resize every frame to the same bbox height
- rejected source if actor, weapon, hair, robe, or limb touches the source cell boundary
- no baked-in attack trails or VFX in actor frames by default
- transparent alpha or removable flat chroma background
- no shadows, text, borders, grids, scenery, or motion blur

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

## Relationship To Other Skills

- Use `$imagegen` behavior implicitly through the built-in `image_gen` tool for bitmap generation.
- Use `/Users/nomofu/.codex/skills/sprite-animation-pipeline` only when the task needs generic sprite-sheet packing, palette quantization, foot-baseline alignment, preview GIFs, or repeated frame postprocessing beyond this project's keyframe prompt workflow.
- Use `$animation-visual-qa` after the frames are integrated into a playable animation and the user wants subjective motion judgment.

## Deliverables

For each completed keyframe task, report:

- job folder
- final prompt summary
- generated frame slots
- accepted/rejected frames
- final workspace paths
- any remaining format or motion risks
