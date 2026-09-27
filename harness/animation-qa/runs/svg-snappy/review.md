# Faster action cadence — 2026-09-26

User feedback: movement still feels sluggish. Compress the whole attack presentation rather than only the approach.

battleTiming.ts maps source frame boundaries and choreography through the same piecewise timing curve. At baseline server durations: approach 95–120ms, strike 65/85ms, hit stop 32/42ms, follow-through 30/40ms, recovery 95/115ms, tail 35ms. Total attacking events are 352–437ms, down from roughly 750–1010ms. Server combat/cooldowns are unchanged; non-attacking effects retain their timing. Impact callbacks and frames remain synchronized, and custom server durations scale the new presentation proportionally. Trails and hit flash now clear by the shorter recovery end.

Visual verdict: Pass for brisker pacing. Reviewed all three 16fps storyboards of the final 6.360s full demo and all three of the 6.480s focused 0.65x recording under harness/tmp/animation-qa/svg-snappy/. Focused sequence covers seven attacks and mobile poses. The full demo's 0–3s segment now includes the first six attacks, with the round head, lean, contact and return still discernible; segment 3–6.36s preserves healing, final sword/palm and settlement. Contact retains a brief stop rather than uniformly accelerating every frame. Dense storyboard inspection does not establish measured frame pacing.

Checks: Svelte check, build, 11 behavior checks and 10 browser scenarios passed. Logic checks verify timing bounds, source-duration scaling, contact frame alignment, hit-stop holds, approach semantics and end-of-event cleanup. A residual hit-flash lifetime exposed by the shortened duration was corrected and the final full/focused recordings captured after that fix.
