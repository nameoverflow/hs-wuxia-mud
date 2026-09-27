# Dedicated approach choreography — 2026-09-26

Request: stand farther apart when idle; travel to the opponent before attacking; give every move its own movement animation. Preserve the accepted round-head artwork and expansive attack poses.

Idle roots are now ±140 (280 source pixels apart). battleApproach.ts maps all seven attacking actions to distinct footwork: guarded step, low palm drive, knee hop, sword glide, crossing step, raised-sword step, low skate. Each has duration, lift and stride settings. sampleApproachPose animates carrying posture and alternating legs; landing blends back to a ready pose before the existing windup. Root movement stops on arrival and stays planted through impact. Recovery adds retreat footwork.

The resolver adds a 280–360 ms presentation lead-in (scaled with server clip duration), shifts impact/launch/recover/rest by that amount, and leaves hit-stop duration unchanged apart from scaling. The original server action duration remains the attack portion. HP/audio callbacks use the shifted impact. Source clip sampling and queue-tail compression now exclude the lead-in from their source-duration calculation. Focus/effect/settlement do not approach. Reduced motion omits travel but preserves event timing.

## Visual verdict: Pass

Reviewed every sheet of the final full-speed demo (10.480 s, 4 sheets) and all seven moves at 0.65× speed (12.360 s, 5 sheets), sampled at 16 fps. Evidence: harness/tmp/animation-qa/svg-approach/{storyboards,focused/storyboards}/. The focused run also captures 390px mobile poses at the end, with gray video padding caused by the viewport resize.

Full segment 0–3 s shows the wider idle space, guarded punch approach, landing then punch, and mirrored enemy sword entry. Segment 3–6 s shows distinct airborne knee entry and planted cut/palm follow-up. Segment 6–9 s shows low sword approach and return spacing, then the final palm. Segment 9–10.48 s preserves settlement.

Focused segments 0–3 s: punch vs sword glide. 3–6 s: crossing sword carry vs raised-sword advance. 6–9 s: low skating approach vs knee hop. 9–12.36 s: crouched palm drive, then mobile sword poses. No clipping in inspected mobile chop preparation. The movement is stylized joint interpolation, not foot-locked physical locomotion; exact frame pacing was not measured.

Validation: Svelte check zero errors/warnings; production build passes; 11 logic checks pass (including all seven distinct approaches, arrival before strike, stationary root at contact, continuous landing, scaled durations, trail synchronization, HP clock semantics). The existing 9 browser tests passed, and a new targeted approach test passed: approach frame/phase is visible, root stops before attack, no early presented damage. A normalized-phase easing mistake discovered by the continuity test was fixed before final recordings.
