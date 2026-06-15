# Animation Visual QA Review Prompt

Expectation:
当前战斗动画应符合 docs/battle-animation.md 的第一版目标：黑底剪影舞台，两个高对比小人，普通剑法刺击应短促清楚，攻击者快速前压，红色直线剑光在命中阶段出现，敌人有明显受击反馈，伤害飘字清楚，整体轻量但有打击感，不应拖沓、漂浮或遮挡。

Artifacts:
- recording: /Users/nomofu/code/hs-wuxia-mud/harness/tmp/animation-qa/current-battle/stage-recording.webm
- storyboard: /Users/nomofu/code/hs-wuxia-mud/harness/tmp/animation-qa/current-battle/stage-storyboards/storyboard-001.png
- storyboard: /Users/nomofu/code/hs-wuxia-mud/harness/tmp/animation-qa/current-battle/stage-storyboards/storyboard-002.png

Instructions for Codex:

1. Inspect every storyboard image with view_image in listed order.
2. Treat each storyboard as a time sequence, reading left-to-right and top-to-bottom.
3. Judge whether the animation matches the expectation subjectively.
4. Focus on action semantics, rhythm, impact, continuity, composition, and wuxia silhouette style.
5. Return:
   - Verdict: Fail, Borderline, or Pass
   - Score: 0-100
   - Findings with segment/frame-region evidence
   - Recommended animation changes
