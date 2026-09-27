# Broad wuxia movement — 2026-09-26

Request: retain the round-headed figures but make their movement expansive, spectacular and elegant.

Changes: root separation 136 → 184 source pixels; deeper rear-leg preparation, full bow stances, counterbalancing extended arms, overhead chop, low-to-high rising cut, raised-knee side kick. Sword length 53 → 72, while its tip still meets the manifest contact point. Each attack continues into a separate follow-through instead of reversing directly into idle. The downward sword follow-through was raised after review to keep the blade above the ground. Kick/rising release has a small vertical arc returning to ground at impact. Dodge withdraws farther. Camera emphasis remains small (1.5%/3.5%). Fine trails sample the actual historical weapon/limb positions and two low-opacity afterimages; all use the same held clock. No server durations, impact markers, damage or queue rules changed.

## Review: Pass for the requested direction

Compared with the previous round-head storyboard: action silhouettes occupy appreciably more space, and off-hand/leg counterbalance remains visible. Reviewed all three sheets of the 7.920-second full demo at 16 fps, then all three sheets of the final 8.400-second focused half-speed recording (cut, chop, rising cut, kick, heavy palm plus mobile frozen poses). Paths are under harness/tmp/animation-qa/svg-wuxia/ and focused/.

Focused segment 0–3 s: cut opens behind the actor and travels in a long sweep; overhead chop has a readable held preparation and independent low follow-through. The revised blade tip stays above the ground on recovery.
Focused segment 3–6 s: rising cut gathers overhead after contact; kick folds the knee before extending and folds it again to recover. Main silhouettes remain clear over the faint echoes.
Focused segment 6–8.4 s: heavy palm stretches into a bow stance; mobile chop/rising poses remain inside the stage. Final desktop preview.png shows the sword arc, extended stance and opponent guard.

Mobile checks use a 390-pixel viewport. The focused video changes viewport for frozen mobile captures near its end, producing gray video padding outside the viewport; this is a capture artifact. Initialization samples precede the action. Storyboards verify visible pose progression and composition, not exact frame pacing or all-device performance. Joint interpolation still permits limb-length changes and is not an IK solver.

Validation: zero Svelte check errors/warnings; 10 battle behavior checks pass, including weapon contact, held trails/echoes and reduced-motion suppression; production build passes. All 9 browser scenarios passed after the motion/trail integration. The final follow-through angle adjustment was checked with another complete focused recording and logic/build checks. No backend encounter or deployment claimed.
