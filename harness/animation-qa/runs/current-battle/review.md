# Current Battle Animation Visual QA

Verdict: Borderline
Score: 68/100

## Expectation

The current battle animation should match the first-pass direction in `docs/battle-animation.md`: a dark silhouette stage, two high-contrast fighters, a short and clear normal sword stab, quick forward pressure from the attacker, a red straight sword trail during impact, visible defender hit feedback, clear damage float text, and a lightweight but impactful wuxia feel without dragging, floating, or visual overlap.

## Artifacts

- Full recording: `harness/tmp/animation-qa/current-battle/recording.webm`
- Stage crop recording: `harness/tmp/animation-qa/current-battle/stage-recording.webm`
- Full page storyboards: `harness/tmp/animation-qa/current-battle/storyboards/storyboard-001.png`, `harness/tmp/animation-qa/current-battle/storyboards/storyboard-002.png`
- Stage storyboards: `harness/tmp/animation-qa/current-battle/stage-storyboards/storyboard-001.png`, `harness/tmp/animation-qa/current-battle/stage-storyboards/storyboard-002.png`

## Findings

- [major] The attack reads as a horizontal sword stab, and the left-to-right direction is correct, but the attacker appears to slide forward more than lunge. In `stage-storyboards/storyboard-001.png`, row 2 through row 3 shows a smooth translation with limited body anticipation or snap, so the hit has less explosive "short thrust" energy than expected.
- [major] The red straight trail is visible and correctly aligned with the stab, but it persists across too many neighboring frames. It starts feeling like a static red extension of the weapon instead of a brief impact flash.
- [major] The defender feedback is present, but subtle. The enemy leans back and the damage number appears, yet the hit reaction does not strongly separate from the attacker's forward pose. The impact would benefit from a sharper recoil, flash, or hit spark near the enemy body.
- [minor] The `-18` damage float is legible in the cropped storyboard, but in the full-page storyboard it is small and easy to miss. The text placement is acceptable, but the font size or contrast may need to scale with the battle-stage size.
- [minor] Recovery is clean. `stage-storyboards/storyboard-002.png` shows both actors return to idle without lingering trail or damage text artifacts.
- [minor] The visual style matches the intended dark silhouette direction, but the yellow silhouettes read closer to small UI icons than ink-black fighters. This may be acceptable for first pass, but it weakens the wuxia atmosphere described in the design doc.

## Recommended Changes

- Make the stab timing more front-loaded: shorten windup, make the strike phase faster, and let recover take the remaining time.
- Reduce trail persistence and increase peak brightness around the impact frame only.
- Strengthen hit feedback with a brief enemy recoil distance, white/red flash, or larger spark at the defender.
- Slightly enlarge or brighten damage float text for full-page readability.
- Consider changing the actor palette or treatment if the goal is "black silhouette" rather than "golden sprite on dark stage".
