# Round-head SVG correction — 2026-09-26

User feedback supersedes the earlier style verdict: the robed, side-profile SVG characters were too far from the original round-headed figures.

References inspected: raster-v1/fist/body/idle.png, punch_strike.png, sword/body/sword_ready.png, fist/hair/idle.png. Keep large featureless oval heads, rounded solid limbs, simple torso and optional wavy ponytail. Remove nose/chin, topknot, robe, belt, sash, boots and layered limb opacity. Elbows/knees now use rounded polyline joints instead of bending the entire limb into a curve. Chambered fist stays below the enlarged head.

Visual verdict: Pass for the requested round-head direction, subject to user preference. Inspected live idle/punch screenshots and all three 16 fps storyboards of the 7.880-second full demo under harness/tmp/animation-qa/svg-round-head/. Segment 1 (0–3 s) shows readable round heads during punches, guard and raised-leg kick; segment 2 (3–6 s) retains the silhouette during healing, sword counter and final contact; segment 3 shows settlement/fade. The initial two recording samples capture page initialization, before the staged demo. No exact frame-rate measurement claimed.

Validation: Svelte check has zero errors/warnings; 9 battle behavior checks pass; production build and git diff --check pass. Timing/queue logic remains unchanged. Full browser regression from the preceding version was not rerun for this shape-only revision; the new live recording exercised the complete demo.
