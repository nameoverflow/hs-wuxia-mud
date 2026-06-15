# Combat Keyframe Frame Format

Use this reference when generating or accepting battle animation keyframes for `hs-wuxia-mud`.

## Current Project Target

Existing actor frames are:

```text
client/src/assets/battle/actors/**/*.png
PNG RGBA
256 x 192
one actor pose per file
transparent background
```

Generated source images may be larger, but final project assets should be normalized to this target unless the consuming code is intentionally changed.

Use `.codex/skills/combat-keyframe-generator/scripts/normalize_keyframes.py` for sheet outputs. It enforces transparent RGBA output, one shared scale per sheet, stable baseline, source-edge rejection, and tiny-fragment cleanup.

Canonical proportion/style reference:

```text
docs/assets/battle-animation/actor-style-anchor-canonical.png
```

Skill asset mirror:

```text
.codex/skills/combat-keyframe-generator/assets/actor-style-anchor-canonical.png
```

Use this as the accepted v13 baseline for compact actor proportions, arm balance, and detail level.

## Hard Requirements

- One actor only per frame unless the user explicitly asks for a multi-actor effect reference.
- Full body visible, including feet and weapon.
- Side-view orthographic 2D game sprite, facing right by default.
- Simplified compact actor shape. Prefer a clean small-game silhouette over detailed character illustration.
- Human-readable body first: head above torso, shoulder/hip mass, two arms, two legs, planted feet, and a plausible action stance. Do not accept broken icon geometry or abstract-logo silhouettes.
- Stable scale across all frames in the same action set.
- Stable foot baseline across all frames. Aim for feet landing near the same y coordinate after resizing.
- Stable body shape, gender marker, weapon, color, modest non-perfect-circle head size, body proportions, and silhouette family.
- Stable arm balance: front raised arm and rear waist-side arm should use similar simplified limb thickness and visual weight. The rear arm must read as a compact bent arm, not a small notch.
- No clothing form. Actor bodies should match the abstract male/female reference images, not robes, uniforms, boots, armor, or costume silhouettes.
- No text, numbers, labels, UI, watermark, frame border, grid, or background scenery.
- No cast shadow, ground shadow, reflection, smoke background, cinematic lighting, or motion blur.
- No baked-in attack trail, weapon glow, sparks, slash arcs, or hit effects in actor frames by default. Generate VFX as separate assets/layers unless explicitly requested.
- Leave enough empty margin for weapon extension and later CSS movement.
- For source sheets, no actor part may touch its invisible source cell boundary. This includes sword tips, ponytails, limbs, and raised feet. Regenerate instead of trying to recover clipped art.
- Alpha background is preferred. A perfectly flat chroma-key background is acceptable as a source if it will be removed locally.

## Simplified Shape Standard

The current first-pass assets are readable but too detailed for long-term skill generation. New actor frames should use a stricter shape language:

- 1-2 flat colors only for the actor body, usually a single warm yellow/gold silhouette plus optional dark cutout gaps.
- No face details: no eyes, mouth, nose, eyebrows, facial highlights, or expression marks.
- Male has no hair. Female has one simple high-tied ponytail mass reaching below the waist line toward the upper hip along the back, with a small tie/knot bump, flowing S-curve, and tapered or subtly split tip. No individual hair strands.
- No garment folds, embroidery, belts, layered trim, fabric texture, boots, sleeves, robe hems, or costume silhouette.
- No gradients, rim-lighting, shine, antialias glow, or internal contour lines.
- Use 5-7 major readable masses at most: head, torso, two arms, two legs, optional weapon, optional female ponytail.
- Use negative space only where it improves small-size readability, such as separating legs or weapon from body.
- Match the canonical anchor's squat compact, low-center, 2.5-3 head-tall feel.
- Male and female actors must share the same total body height, modest head size, torso scale, limb thickness, and foot baseline. The female ponytail is an extra attached marker, not a reason to make the figure taller or more detailed.
- Male may read slightly wider/squarer; female may read slightly lighter/narrower and more upright. Do not exaggerate this into different anatomy or costume design.
- The frame should still read when viewed at roughly `85x64`.

Reject frames that look like concept art, a detailed chibi illustration, a full costume design, kung-fu training wear, any period clothing, generic fantasy armor, broken icon geometry, or an abstract logo instead of a person.

## Recommended Final Geometry

For `256x192` output:

```text
canvas: 256w x 192h
foot baseline: y ~= 170-176
body center x: 118-138 for neutral poses
safe top margin: >= 8px
safe side margin: >= 8px, except long weapons may enter the margin
body height: roughly 118-158px depending on stance
```

Do not fit each frame independently to its bounding box. Use a shared scale for the whole action set so frames do not pop during animation. The default normalizer chooses that scale from the `idle` reference slot, then reduces it only when the largest lunge, kick, sword, or recoil would exceed the final canvas.

When an action set is generated as a sheet, preserve source-sheet visual proportions:

- Prefer component mode: segment the full sheet by chroma-removed actor components, group components into the requested slots, then crop the grouped pose.
- Use grid mode only when the source sheet has reliable deterministic cells.
- Compute one shared scale for all cells in that sheet.
- Paste every scaled frame with the same foot baseline, usually `y=176`.
- Allow crouched or leaning poses to be shorter than idle after scaling; do not stretch them back to idle height.
- Reject any source whose grouped pose touches the sheet edge. In grid mode, also reject any source cell whose alpha bbox touches the cell boundary within a few pixels, because the weapon or body may already be clipped.

## Gender Differentiation Without Detail Creep

Keep male/female distinction stable and subtle. It must survive at small size without adding facial or costume detail.

### Male baseline markers

- plain softened rounded head, not a perfect circle
- no hair
- no clothing marker
- stance may be a little wider or squarer
- balanced front and rear guard arms; rear arm is visible and compact, not a notch

### Female baseline markers

- same abstract body style as male
- one high-tied long solid ponytail, reaching below the waist line toward the upper hip along the back, with a flowing S-curve and tapered or subtly split tip
- no other gender marker
- stance may be lighter, but feet must remain grounded
- balanced front and rear guard arms; raised fist should not dominate the silhouette

Do not use exaggerated anatomy, facial features, ornate hair strands, decorative jewelry, sexualized details, or costume differences. Gender should be a reusable silhouette profile, not a detailed costume.

## Slot-Specific Pose Requirements

### idle

- balanced combat stance
- weapon readable but not at maximum extension
- feet planted on baseline

### windup

- clear preparation in the opposite direction of force
- small downward, backward, or rotational anticipation
- should still read as the same actor and weapon

### strike

- most readable action silhouette
- line of force should be obvious
- weapon and lead limb can extend, but body must remain fully visible

### impact

- optional attacker contact pose
- use only when the contact silhouette differs from strike
- leave space around weapon tip for CSS/VFX layers

### recover

- returning, settling, or softened action pose
- can be skipped if idle works as the recover frame

### target_hurt

- defender recoil must be visible in silhouette
- do not make the actor fall completely out of frame
- keep feet or landing point visible

### target_parry

- stable guarded pose
- arms or weapon clearly intercept incoming force
- body should not recoil like a hit

### target_dodge

- side-step, lean, or afterimage-friendly pose
- should imply avoiding the original attack line
- keep the body readable and not over-crouched

## Rejection Checklist

Reject and regenerate when:

- the model draws multiple characters
- the weapon changes type or disappears
- the actor faces the wrong direction
- feet are cropped or baseline shifts heavily
- every pose has the same output bbox height because a script scaled them independently
- a long weapon reaches the final frame edge or touched the source sheet cell edge
- the frame contains text, UI, border, grid, or background
- the pose is a cinematic illustration instead of a small UI sprite
- the actor has facial features, detailed hair strands, clothing forms, garment folds, gradients, glow, or painterly texture
- attack trails or VFX are baked into an actor frame without explicit request
- the style changes across frames
- the action reads only because of motion blur instead of silhouette

## Anchor Restyle Gate

Before final rejection, classify each failed draft:

- Action fails: regenerate from a better pose/action prompt.
- Action passes but style fails: run one anchor restyle pass.
- Action and style both fail: reject; do not restyle.

Anchor restyle input roles:

- v13 anchor image: identity/style source.
- failed draft: pose-only source.

Accept a restyled frame only if it preserves the action line while matching v13's body family: no clothing form, no round-head drift, no extra hair/costume detail, stable gender marker, readable arms and legs, stable feet, and full weapon visibility.

Store restyled candidates separately from raw action drafts until they pass inspection. Do not normalize or copy a frame into `client/src/assets/battle/actors/...` until it passes both action and style gates.
