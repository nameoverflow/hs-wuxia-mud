# v4 Portrait Style Lock

## Canonical Assets

Locked v4 originals live in:

`docs/assets/character-portraits/wuxia-avatar-beauty-scale-v4/originals/`

Canonical files:

| Score | File | Role |
| --- | --- | --- |
| 0 | `beauty-score-00-source.png` | abstract comedy Jianghu face, zero-attractiveness gag |
| 2 | `beauty-score-02-source.png` | human-ish comic crooked face, foolish smug grin |
| 4 | `beauty-score-04-source.png` | cute pure freckled face with obvious flaws, still harmonious |
| 6 | `beauty-score-06-source.png` | pleasant tea-house disciple, open smile, normal appeal |
| 8 | `beauty-score-08-source.png` | fox-like inn spy, teasing smirk, sly charm |
| 10 | `beauty-score-10-source.png` | high-cold immortal sword disciple, top-tier facial harmony |

Current locked originals are `1254x1254` PNGs. Keep them uncropped and unmodified. The hard composition rules below apply to new v5+ generated portraits; locked v4 originals remain exempt and should not be altered to fit later rules.

## Visual Specification

- **Canvas:** square `1:1` parchment portrait. For generated source assets, prefer a large square original and preserve it as-is.
- **Canonical original:** full square portrait, no UI frame, no text, no watermark, no transparent background.
- **Framing:** tight head-and-collar avatar, not a bust illustration. Include head, hair, neck, robe collar, and tiny shoulder hints only. Do not erase the body into a floating head, but do not show chest, waist, arms, hands, sleeves, or action limbs.
- **Head/body balance for new generations:** face plus hair should occupy about `84-90%` of the square height. Clothing should be a narrow collar strip plus tiny shoulder tips in the bottom `6-12%`. For explicit over-shoulder/back-turn views, collar/shoulder mass may reach `18%`, but anything that reads as half-body or broad torso is a failure. Do not cut off hair buns, hair ornaments, chin, or shoulders.
- **Pose/angle:** allow front, strict symmetrical front, near-front, slight top-down, slight low-angle, lifted chin, reverse-direction over-shoulder, or turned head. Avoid every character defaulting to the same 45-degree half-profile. In a set of 3+ portraits, require at least two clearly different angle families.
- **Linework:** rough hand-drawn Chinese ink, thick uneven dry-brush outlines, visible grain, imperfect but controlled. Avoid thin pencil-like hairlines and polished anime cleanup.
- **Detail level:** low-to-moderate detail. Keep hair masses broad, costume folds sparse, accessories simple, and facial rendering readable rather than hyper-detailed.
- **Color:** aged tan rice paper, black/dark ink, muted flat watercolor wash. Use a few subdued robe or accessory colors; avoid glowing fantasy palettes, cinematic lighting, gradients, and ornate armor.
- **Attractiveness scoring:** use face shape, eyes, brows, nose, mouth, expression, and harmony to encode beauty score. Do not make low scores low because clothes are broken, and do not make high scores high because clothes are luxurious.
- **Face diversity:** vary face model, expression, hair design, personality, and angle between scores. Do not reuse the same face for adjacent high-score characters.

## Prompt Template

Use one prompt per portrait:

```text
Create one original square Jianghu character portrait in the locked v4 style for this project.

Use case: stylized-concept
Asset type: full-size source image for a wuxia MUD character portrait set
Character target: <score/role/personality>
Beauty-score intent: <how the face itself communicates score/personality>
Style: rough old Chinese mobile wuxia portrait, thick uneven black ink brush lines, dry-brush texture, muted flat watercolor wash on aged tan rice paper, low-to-moderate detail, hand-drawn and slightly imperfect but not careless
Composition: full square portrait, tight head-and-collar avatar, not a bust illustration; face plus hair fill about 84-90% of the square; only show neck, a narrow robe-collar strip, and tiny shoulder hints in the bottom 6-12%; no chest, torso, arms, hands, sleeves, waist, or broad shoulder mass; no crop; no UI frame
Pose/angle: <strict symmetrical front / near-front / slight top-down front / slight low-angle / lifted chin / reverse-direction over-shoulder / turned-head but not same-side 45-degree repetition>
Face diversity: <face shape, eyes, brows, nose, mouth, expression, and how this differs from nearby scores>
Clothing: normal Jianghu cloth robe with restrained variation; simple hair tie or hairpin only if appropriate
Constraints: preserve a complete square original; do not crop; do not downsample; do not create a small icon export; no text; no logo; no watermark; no transparent background
Avoid: polished anime, photorealism, 3D render, thin pencil lines, excessive hair strands, highly rendered skin, ornate fantasy armor, glamorous costume as the reason for beauty, broken clothing as the reason for ugliness, large upper body, chest-heavy bust, visible torso, broad shoulders, arms, hands, waist-up action pose, repeated same-side 45-degree half-profile, complex scenery, UI border
```

Add this composition block verbatim to every new v5+ generation prompt:

```text
STRICT COMPOSITION: square head-and-collar portrait only. Face plus hair must visually occupy about 84-90% of the image height. Clothing is only a narrow robe-collar strip plus tiny shoulder hints in the bottom 6-12%; for an explicit over-shoulder/back-turn view it may reach 18% at most. No chest, torso mass, waist, broad shoulders, arms, hands, forearms, elbows, full sleeves, or action-pose limbs. Do not turn it into a bust, half-body, waist-up portrait, or cropped UI icon.
```

For stricter angle control, add one of these blocks verbatim:

```text
STRICT ANGLE: dead-front symmetrical face, like a formal front-facing portrait. Nose exactly centered, both eyes the same size, both cheeks visible equally, both side hair masses balanced. No head turn. No 3/4 view. No 45-degree half-profile. No side profile.
```

```text
STRICT ANGLE: reverse-direction over-the-shoulder view. Her collar/shoulder turns toward the viewer's left while her face looks back over the opposite shoulder toward the viewer. The dominant cheek and hair mass must be on the opposite side from the common same-side 45-degree portrait. Do not use the usual 45-degree half-profile.
```

```text
STRICT ANGLE: slight top-down front view. The viewer is a little above her; she tilts her chin down slightly and looks up at the viewer. This must read as front/top-down, not side profile, not 45-degree half-profile.
```

## Score Guidance

- `0`: can be abstract and comic, almost non-attractive gag, but still in parchment ink style.
- `2`: human-ish comic distortion, intentionally goofy and unattractive.
- `4`: normal, cute/pure, harmonious overall, with visible facial flaws such as freckles and a slightly upturned nose. Should not become an 8-level refined beauty.
- `6`: pleasant and approachable, normal attractive, not glamorous.
- `8`: clearly beautiful with distinct personality, e.g. sly, fox-like, lively, or teasing.
- `10`: strongest facial harmony and presence, not just fancier clothing; can be cold, immortal, iconic, or commanding.

## QA Checklist

Run the QA script and inspect the contact sheet plus individual originals.

Reject or regenerate if any of these fail:

- source image is not a square full original
- subject is cropped, head/hair/shoulders cut off, or only a floating face remains
- face plus hair does not occupy roughly `84-90%` of image height for new generated portraits
- clothing/upper body is more than the bottom `6-12%` for normal views, or more than `18%` for explicit over-shoulder/back-turn views
- visible chest, waist, torso mass, broad shoulder block, arms, hands, sleeves, or action pose appears
- linework is too fine, too polished, too digital, or too hyper-detailed
- linework is so careless that the face becomes unintentionally ugly for scores 4+
- background is not plain aged parchment
- clothing drives the beauty score instead of facial features
- scores 6/8/10 reuse the same face, hair, or expression
- 4 becomes too refined or too beautiful
- all angles collapse into the same 45-degree half-profile
- a front-view prompt still produces a 3/4 or 45-degree face
- an over-shoulder or reverse-direction prompt still produces the usual same-side 45-degree face
- image includes UI frame, labels, watermark, logo, scenery, modern props, or fantasy armor

Accept only after writing a short QA note with:

- overall verdict: `Pass`, `Borderline`, or `Fail`
- score/personality match
- style match against v4 anchors
- framing/head-body note
- any rejected variants and why
