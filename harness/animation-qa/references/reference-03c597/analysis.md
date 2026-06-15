# Reference Combat Animation Analysis

Source: `harness/animation-qa/references/reference-03c597/reference.mp4`

Video facts:

- Duration: 16.7s
- Resolution: 592x1280
- Video stream: HEVC, about 30fps, 500 frames
- The final ~1.5s is iOS control center overlay and should be ignored for combat animation reference.

## Storyboards

- 2s center crop overview: `harness/tmp/animation-qa/reference-03c597/storyboards-center-2s/storyboard-001.png` through `storyboard-009.png`
- 1s / 24fps center crop detail: `harness/tmp/animation-qa/reference-03c597/storyboards-center-hires/storyboard-001.png` through `storyboard-017.png`
- Full raw copied video: `harness/animation-qa/references/reference-03c597/reference.mp4`

## Main Observation

This animation does not appear to rely on many fully drawn character frames. It gets most of its natural feel from a small number of held silhouette poses, sharp non-linear movement, short-lived effect layers, damage-number timing, and deliberate pauses between bursts.

The characters often hold the same pose for many consecutive frames. When action happens, the actor travels quickly over a short interval, effects appear near the contact frame, then the scene settles back into a readable idle state. This is a good fit for a lightweight MUD combat UI.

## Why It Feels Natural

- The timing has strong contrast: long readable holds, then a short attack burst. In `storyboards-center-hires/storyboard-002.png`, the first half is mostly held pose, then the attacker rapidly crosses distance and impact appears near the end of the second.
- The motion is not evenly linear. Attacks feel like they accelerate into contact, overshoot slightly, then settle. This avoids the "sprite sliding at constant speed" look.
- Attack readability comes from direction-specific trails. Straight red lines sell thrusts; white/blue arcs sell slashes, parries, or heavier impacts.
- Impact is multi-layered: body contact, small flash/spark, damage number, and sometimes a secondary blue/white arc. The actor sprite itself changes little, but the effect stack makes the hit feel active.
- Damage text is synchronized with contact rather than appearing before motion. It appears around the hit frame, then holds long enough for the player to read it.
- Text log updates provide rhythm. Combat does not need continuous motion; the animation alternates between burst, readable text update, and idle hold.
- The same small action vocabulary is reused many times with variations in side, trail shape, damage number, and pause length. The viewer reads these as different actions even when actor poses are sparse.

## Minimal Keyframe Recipe

For this project, a similar feel can be approximated with few actor poses if each attack is split into layered timelines.

Actor poses per weapon/action:

- idle
- windup or ready lean
- strike extension
- recover/settle, often just idle reused
- hurt/recoil
- parry/dodge, optional shared feedback poses

CSS/DOM motion keyframes per attack:

```text
0%    idle hold / readable start
18%   windup lean, small backward or downward preparation
34%   fast dash begins
46%   contact / maximum extension / trail peak / damage appears
58%   recoil or overshoot settle
100%  idle, effects gone, damage fading or gone
```

Approximate timing:

```text
pre-hold:       120-300ms, optional when chaining
windup:          80-160ms
dash/strike:     80-140ms
impact hold:    100-180ms
recover:        220-420ms
between events: 300-900ms depending on combat log pacing
```

Effect layers should have their own shorter keyframes:

- red thrust line: appears around contact, peak for 2-5 frames, fades quickly
- slash/parry arc: appears around contact, stays for 4-8 frames, fades and drifts
- hit spark: very short, 2-4 frames
- damage text: appears at contact, holds 250-500ms, then fades/drifts

## What This Means For Current Battle Animation

The current project can reach a similar natural result without full sprite animation. The current `BattlePanel` already has the right basic ingredients: idle/attack/hurt sprites, red trail, damage float, and a dark stage. The gap is mostly timing and effect layering:

- Make movement less linear and more bursty.
- Add a small windup/anticipation before the dash.
- Make the strike phase shorter and sharper.
- Make trails peak only at contact instead of staying as a weapon extension.
- Strengthen defender feedback with a brief recoil, flash, or spark.
- Let the combat log and animation share timing so each burst has a readable pause.

## Feasibility

Yes, this style is achievable with very few key actor frames. A practical first target is:

- 3 attack poses for sword: stab, slash, uppercut
- 3 shared defender poses: hurt, parry, dodge
- 3-5 CSS keyframes per action
- 3-4 independent effect timelines per cue

The most important improvement is not adding more character frames; it is making the timing non-linear and making impact frames visually dense.
