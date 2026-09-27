# Fist action keyframe reference review

## Purpose

The generated sheet is a pose reference for the segmented-v12 skeleton, not a final raster sprite export. It establishes six clearly different silhouettes: punch windup/strike, heavy windup/strike, and kick windup/strike.

## Accepted reference

- `generated/fist-six-pose-reference-sheet.png`
- All figures face right and keep the compact project actor proportions.
- Punch reads as a direct lead-hand extension.
- Heavy strike reads as a deeper, two-arm body-driven release.
- Kick reads as chamber followed by maximum leg extension.

## Runtime landing

The reference was translated into the six skeletal poses in `client/src/battle/skeletal/data/segmented-v12-poses.json`. The action manifest uses two discrete held frames per move, with the strike frame selected as `impactFrame`. Male and female use these same poses; the female profile only enables the ponytail binding.
