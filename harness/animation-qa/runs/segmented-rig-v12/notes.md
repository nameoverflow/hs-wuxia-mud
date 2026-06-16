# Segmented Rig v12

- Parts source: user-provided `image.psd(1).png`, preserved here as `generated/segmented-rig-v12-parts-source.png`
- Bone/keypoint source: user-provided `image.psd(2).png`, preserved here as `generated/segmented-rig-v12-bones-source.png`
- Generated parts: `client/src/assets/battle/actors/segmented/v12/*.png`
- Manifest: `harness/animation-qa/runs/segmented-rig-v12/generated/segmented-rig-v12-manifest.json`
- Source-scale preview: `harness/animation-qa/runs/segmented-rig-v12/generated/segmented-rig-v12-source-scale-preview.png`
- Anchor reference: `harness/animation-qa/runs/segmented-rig-v12/generated/segmented-rig-v12-anchor-reference.png`
- Anchor target transform: `harness/animation-qa/runs/segmented-rig-v12/generated/segmented-rig-v12-anchor-target.json`

The parts are mechanically extracted from the user-provided yellow-on-black sheet: black background is converted to alpha and each connected yellow component is cropped. No shape redraw is applied.

The skeleton source is detected from the blue nodes in the updated overlay image. It has 30 nodes total, including the corrected rear-leg knee. Limbs are bound as five-point skeleton-grid warp meshes:

- arms: root, shoulder, elbow, wrist, hand
- legs: root, hip, knee, ankle, foot
- ponytail: root, fixed base, mid, lower, tip

After component isolation, `leg_back.png` and `ponytail.png` are clean single connected parts. The limb renderer now maps a small source-image grid through the source and target skeleton paths, so visible shape comes from the original PNG alpha instead of inferred cross-section contours.

The animation rig tool keeps `Lock lengths` enabled by default and now supports constrained dragging for five-point deform chains, so editing hand/foot terminal points preserves the upstream segment lengths.

The `segmented.v12` baseline was reworked after the initial v12 attempt because using per-part scale values broke the source proportions. The current `bind` and `idle` poses are the anchor-reference baseline:

- all parts use one shared source scale, `0.2`
- the anchor reference mask bbox is `[327, 280, 821, 948]`
- the canvas transform is `x * 0.2 + 13.6666`, `y * 0.2 - 13.6`
- the rig root remains at `(128, 176)`, with the reference feet aligned to the canvas baseline
- bone lengths and source keypoint distances are derived from the anchor target placements in `segmented-rig-v12-anchor-target.json`
