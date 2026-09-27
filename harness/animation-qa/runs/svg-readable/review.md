# Readable holds with sharp transitions — 2026-09-26

User feedback: the discrete version is too rushed to read. Preserve its sparse poses and sharp transitions, but add time to the important silhouettes instead of restoring continuous interpolation.

Baseline entry is now 140–165ms, with its first half held in compression and its second half a 70–83ms burst. Anticipation window 110/130ms, hit stop 55/70ms plus strike-pose hold 50/60ms, recovery 120/140ms, tail 90/100ms. Total light attacks 565–580ms, heavy attacks 655–665ms. Queue compression now preserves 90ms of tail, so a burst of queued events cannot eliminate all breathing space. Pose shapes and discrete transitions are unchanged.

Visual verdict: Pass for increased pose readability while retaining abrupt attacks; user preference remains the final art-direction judgment. Reviewed all three 16fps sheets of the 8.080s full-speed capture under harness/tmp/animation-qa/svg-readable/. Segment 0–3s shows loaded dash silhouettes, held full-extension punches, guard and kick, with visible idle gaps. Segment 3–6s shows sword/palm contact holds, healing and the final attacks. Segment 6–8.08s shows final damage followed by settlement. Contact poses occupy multiple consecutive samples rather than disappearing immediately. The omitted 12ms breakdown remains shorter than the sample interval; no exact frame-pacing claim.

Validation: zero Svelte diagnostics, build passes, 11 logic checks and 10 browser tests pass. Logic assertions now protect at least 100ms of anticipation/contact-pose visibility and at least 90ms tail alongside contact synchronization, held key poses, dash arrival, reduced motion and queue ordering.
