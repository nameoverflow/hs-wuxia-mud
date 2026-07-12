# Raster keyframes v1 review

## Scope

- Replace the runtime segmented skeleton with generated discrete body frames.
- Keep the existing action IDs, per-frame holds, impact timing, actor motion, target reactions, and VFX.
- Share every body frame between male and female profiles; female adds only a pose-matched ponytail layer behind the body.

## Generation decisions

- Fist body: the first sheet was rejected because pose spacing and scale were not consistent enough. `source-sheet-v2.png` is the accepted source.
- Sword body: the first two sheets were rejected for crowded framing and a clipped rising strike. `source-sheet-v3.png` is the accepted source.
- Hair: each accepted body sheet was edited with cyan ponytails only. `extract_hair_overlays.py` then mechanically isolated, recolored, scaled, and aligned those pixels with the corresponding accepted body frame.
- Chroma removal, shared scaling, baseline alignment, padding, and 256x192 export were deterministic post-processing; no anatomy or pose was redrawn in code.

## Semantic frame mapping

Fist frames cover idle, guard, hurt, dodge, punch windup/strike, heavy windup/strike, and kick windup/strike.

Sword frames cover ready, parry, hurt, dodge, plus close/prep/strike phases for thrust, downward chop, horizontal cut, and rising cut.

## QA verdict

- Silhouette: pass. Each attack has a readable preparation and maximum-action pose.
- Action semantics: pass. Punch, heavy strike, kick, thrust, downward chop, horizontal cut, and rising cut read as distinct actions.
- Anatomy and balance: pass at battle scale. Weight stays over a plausible support foot and limbs do not require runtime deformation.
- Weapon continuity: pass. The jian remains attached to the hand and inside the 256x192 frame in accepted sword poses.
- Gender variant: pass. Male and female use identical body PNGs; female-only hair is aligned behind the head/body and follows each pose.
- Runtime format: pass. All accepted exports are 256x192 RGBA PNGs with transparent backgrounds and a common foot baseline.
- Timing review: pass by dense 16 fps storyboard sampling. This verifies frame order and impact readability, but is weaker evidence than a captured live battle recording for stage motion and VFX synchronization.

Preview artifacts are intentionally written to `harness/tmp/raster-v1/` because they are regenerable execution output.
