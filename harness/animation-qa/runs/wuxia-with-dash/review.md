# Broad attacks restored, distance dash retained — 2026-09-26

User prefers the earlier expansive version and explicitly clarified that the long-distance dash must remain. Restore the SVG-wuxia attack/reaction interpolation, independent follow-through, original manifest attack timing, historical weapon-tip trails, and 45ms queue-tail compression. Remove battleTiming.ts and later key-pose snapping/retiming. Retain ±140 idle roots, per-action forward-leaning dash, mobile scaling, and source-time correction for the extra lead-in. This is the requested combination, not an exact rollback of every field.

Dash ends in the matching preparation pose, which is held until the original attack launch, avoiding a reset to idle at arrival. The original cubic strike, follow-through and smooth return are restored. Previous latest runtime files were copied to harness/tmp/animation-qa/restore-wuxia/previous before the restoration.

Visual verdict: Pass for the requested combination. Reviewed all four 16fps sheets of the 9.120s complete demo at harness/tmp/animation-qa/wuxia-with-dash/. Segment 0–3s shows far idle spacing, forward-lean entry, smooth full punch and mirrored sword approach. Segment 3–6s shows raised-leg kick, broad sword counter/palm and healing. Segment 6–9.12s shows rising sword, final heavy palm and settlement. Long-range entry and independent broad attack recovery are both visible. No measured frame-pacing claim.

Validation: Svelte check and production build pass, 10 battle logic checks and 10 browser scenarios pass. Tests cover original server attack duration plus dash offset, contact alignment and hit stop, exact-once HP updates, queue/settlement, mobile reduced motion, SVG seeking, and dash arrival preceding the broad attack.
