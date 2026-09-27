# SVG silhouette trial — 2026-09-26

Request: replace battle presentation with SVG character silhouettes. The explicit SVG request authorizes code-native artwork for this trial. Existing unrelated work was preserved; the previous stage and asset loader were copied into ignored harness/tmp/svg-battle-backup before edits.

Implementation: SVG robe/limb/head/hair/weapon shapes, interpolated semantic poses, SVG trails/impact/parry/aura. Existing BattleClock, director, event queue, HP callbacks and settlement remain authoritative. Background remains the generated WebP. Legacy manifest frameset and old raster resources remain for compatibility; runtime uses SVG poses and no actor/VFX atlas loading.

## Visual review: Pass for a working style trial

Evidence: harness/tmp/animation-qa/svg-battle/recording.webm (7.680 s), storyboards/storyboard-001.png through storyboard-003.png, sampled at 16 fps in three-second segments. Viewed all three sheets in sequence.

- Segment 1 (0–3 s): repeated punches have preparation, extension, contact and return. Parry is a planted guard; kick is a distinct raised-leg silhouette; dodge withdraws the target with a brief ghost. Sword counterattack reads in the opposite direction.
- Segment 2 (3–6 s): heavy contact, healing ring and number, enemy evasion, final damage and settlement appear in the expected order. Effects remain localized rather than covering both bodies.
- Segment 3 (6–7.680 s): victory overlay and defeated actor fade remain readable.
- Feet remain at the stage baseline except the deliberately lifted kicking foot. Gold/green figures remain legible against the retained landscape.

The art is deliberately simple, closer to cut-paper figures than detailed character illustration. Joint interpolation can stretch limb lengths; it is not an IK rig. Storyboards do not establish exact frame pacing or full-device performance. No live backend encounter was needed or claimed: the lab and main-game fixture use the actual client event queue.

## Verification

- npm --prefix client run check: no errors or warnings.
- npm --prefix client run test:battle: 9 checks, including SVG contact coordinates, continuity at impact and frozen articulated pose during hit stop.
- npm --prefix client run build: passed; no actor/hair/VFX atlases in build output.
- PLAYWRIGHT_CHANNEL=chrome npm --prefix client run test:battle:browser: browser scenarios cover HP timing, repeated events, dodge, healing, settlement, main-game mount, hidden tabs, sound, mobile reduced motion, and SVG pose seeking/no atlas requests.

Default Playwright Chromium was not installed; verification used installed Chrome through the documented channel override.
