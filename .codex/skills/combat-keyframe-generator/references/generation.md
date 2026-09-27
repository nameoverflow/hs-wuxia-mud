# Keyframe Generation and Anchor Restyle

Use for new artwork or a pose-preserving style correction. Commands run from the repository root. Read only the applicable sections.

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
  --out harness/animation-qa/runs/cold-rain-thrust \
  --name cold-rain-thrust \
  --action "a short cold-rain sword thrust, restrained but sharp" \
  --actor "project baseline sword actor" \
  --identity-profile male \
  --frames idle,windup,strike,impact,recover,target_hurt
```

3. Read the generated `prompts.md`, [frame-format.md](frame-format.md), and [character-identity.md](character-identity.md).
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
10. Normalize accepted generated sources with `scripts/normalize_keyframes.py` using [normalization.md](normalization.md), or an equivalent shared-scale process. Do not fit each frame independently to its own bounding box.
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
