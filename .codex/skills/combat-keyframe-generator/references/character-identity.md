# Unified Combat Actor Identity

Use this reference to keep generated battle keyframes visually unified across future skills.

## Principle

One weapon/user archetype should keep the same body, costume, weapon, scale, facing, and detail level across all skills. A new skill should change pose and VFX, not create a new actor design.

Use these baseline cards unless the user asks for a new actor family.

Canonical visual anchor:

```text
docs/assets/battle-animation/actor-style-anchor-canonical.png
```

Skill asset mirror:

```text
.codex/skills/combat-keyframe-generator/assets/actor-style-anchor-canonical.png
```

Use these images as the style/proportion references for baseline actors. The accepted v13 direction is abstract little-person silhouettes: no clothing form, no facial detail, no costume markers, and balanced front/rear arm weight.

Male reference:

```text
docs/assets/battle-animation/actor-style-reference-male.png
```

Female reference:

```text
docs/assets/battle-animation/actor-style-reference-female.png
```

Skill asset mirrors:

```text
.codex/skills/combat-keyframe-generator/assets/actor-style-reference-male.png
.codex/skills/combat-keyframe-generator/assets/actor-style-reference-female.png
```

## Baseline Style Card

```text
Simplified compact abstract wuxia little-person silhouette for a small dark MUD battle stage.
Flat warm yellow/gold body silhouette, optional tiny dark cutout gaps for limb separation.
No facial features, no clothing silhouette, no internal costume drawing, no texture, no gradients, no shadows.
Side-view orthographic 2D game sprite, facing right. Squat compact 2.5-3 head-tall proportions, low center of gravity, broad readable action shapes.
Full body visible, feet on a stable baseline, readable at small UI size.
The silhouette must still read as a person: head above torso, readable shoulder/hip mass, two arms, two legs, planted feet, and a plausible martial stance. Do not accept broken icon geometry or logo-like abstract shapes.
Actor frame only: no attack trail, no glow, no spark, no damage number, no background.
Male is a plain featureless figure with a softened rounded head that is not a perfect circle. Female is the same body family with one simple high-tied long ponytail reaching below the waist line toward the upper hip along the back, with a small tie/knot bump, a flowing S-curve, and a tapered or subtly split tip. Male and female actors use the same total height, same modest head size, same torso scale, same limb thickness, and same foot baseline. The female ponytail is the only gender marker; do not add clothing, anatomy, face, or hair-strand detail.
Both profiles must keep the v13 arm balance: the raised front arm is not oversized, and the rear waist-side arm reads as a complete compact bent arm, not a tiny notch.
Avoid all clothing forms: robe, sleeve, belt, boots, shoes detail, armor, uniform, costume silhouette, garment folds, hair strands, face, eyes, mouth, and realistic anatomy.
```

## Male Sword Actor Card

```text
Male baseline sword actor.
Simple compact full-body abstract little-person silhouette with a plain softened rounded head, simple torso, balanced front/rear arms, simple legs, no hair, slim straight jian sword if the action needs a sword.
Minimal shape language: head, torso, two arms, two legs, optional sword.
No face, no clothing, no folds, no hair, no ornaments, no glow.
```

## Male Fist Actor Card

```text
Male baseline fist actor.
Simple compact full-body empty-hand abstract little-person silhouette with a plain softened rounded head, simple torso, balanced front/rear arms, simple legs, no hair and no weapon.
Minimal shape language: head, torso, two arms, two legs.
No face, no clothing, no folds, no hair, no ornaments, no glow.
```

## Female Sword Actor Card

```text
Female baseline sword actor.
Simple compact full-body abstract little-person silhouette with simple softened rounded head, one high-tied long ponytail reaching below the waist line toward the upper hip along the back with a flowing S-curve and tapered or subtly split tip, simple torso, balanced front/rear arms, simple legs, slim straight jian sword if the action needs a sword.
The ponytail is one solid shape and the only gender marker.
Minimal shape language: head, ponytail, torso, two arms, two legs, optional sword.
No face, no clothing, no folds, no hair strands, no ornaments, no glow.
```

## Female Fist Actor Card

```text
Female baseline fist actor.
Simple compact full-body empty-hand abstract little-person silhouette with simple softened rounded head, one high-tied long ponytail reaching below the waist line toward the upper hip along the back with a flowing S-curve and tapered or subtly split tip, simple torso, balanced front/rear arms, simple legs, no weapon.
The ponytail is one solid shape and the only gender marker.
Minimal shape language: head, ponytail, torso, two arms, two legs.
No face, no clothing, no folds, no hair strands, no ornaments, no glow.
```

## Neutral Enemy Actor Card

```text
Neutral small enemy silhouette.
Simple full-body abstract opponent silhouette with compact body mass, plain weapon or bare hands as requested, no detailed identity markers.
Readable at small size, same flat style and baseline as player actors.
```

## Continuity Rules

- Before generating, load the correct reference image with `view_image` so image generation can preserve the actual body shape, not only a text description.
- Keep generation context clean: immediately before `image_gen`, display only the selected v13 anchor reference for the actor identity. Do not display old sprites, rejected sheets, runtime previews, or detailed concept art in the same generation flow unless the user explicitly asks to use them as edit targets.
- Do not reference dirty images in generation prompts with phrases such as "ignore the older sprite shown above"; that still makes the dirty image salient. Put those comparisons in QA notes after generation instead.
- After generating an action draft, judge action and identity separately. If the action is good but the actor identity drifted, do not discard the pose immediately: run an anchor restyle pass with exactly two visible references, the v13 anchor as identity/style reference and the flawed draft as pose-only reference.
- During anchor restyle, preserve only the draft's pose, sword/off-hand direction, body lean, and composition. Replace all identity and style details with the anchor; do not preserve clothing-like limbs, round-head drift, garment edges, hair detail, texture, or old rendering style from the draft.
- Reuse the same profile card for all frames in a job.
- Reuse the same profile card for future skills unless a new actor family is intentionally introduced.
- Keep male and female variants in separate reusable profile folders. Do not mix genders in one animation pool unless the runtime has an explicit visual-profile selector.
- Match the canonical anchor's squat compact 2.5-3 head-tall body ratio and abstraction level before adding any action-specific variation.
- Keep sword length, head size, body height, arm balance, ponytail length, and foot baseline stable.
- Generate actor frames without VFX. Generate slash arcs, thrust lines, hit sparks, and parry arcs separately.
- Reject any frame where the body only satisfies geometry constraints but no longer reads as a human figure.
- If a generated frame adds extra detail, reject it even if the pose looks good.
