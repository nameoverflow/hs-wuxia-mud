# 半回合制战斗动画系统

当前战斗角色使用 AI 生成、机械规范化的离散 PNG 关键帧，不再使用分件骨骼、mesh deformation 或运行时 Canvas 重绘。

设计目标：

- 每个 `attack_move` 和 `active_skill` 固定绑定一个 action。
- action 由少量 `{ frameId, holdMs }` 构成，并用 `impactFrame` 标记命中帧。
- 男版和女版共用完全相同的 body PNG；女版只在 body 后方增加逐帧 ponytail overlay。
- 人物帧只表达姿态。接近、突进、后退、命中特效、飘字和目标反馈继续由 Pixi/GSAP 驱动。
- server 决定战斗事实，client 只播放最终 `actionId`。

## 源码入口

- 动作 manifest：[resources/scripts/combat_actions/battle-actions.json](../resources/scripts/combat_actions/battle-actions.json)
- action catalog：[client/src/battle/battleActionCatalog.ts](../client/src/battle/battleActionCatalog.ts)
- PNG frame catalog：[client/src/battle/frameCatalog.ts](../client/src/battle/frameCatalog.ts)
- 离散采样器：[client/src/battle/animationClip.ts](../client/src/battle/animationClip.ts)
- timeline resolver：[client/src/battle/animationResolver.ts](../client/src/battle/animationResolver.ts)
- Pixi actor：[client/src/battle/pixiFrameActor.ts](../client/src/battle/pixiFrameActor.ts)
- Pixi battle stage：[client/src/components/PixiBattleStage.svelte](../client/src/components/PixiBattleStage.svelte)
- 资源校验：[client/scripts/validate-animation-data.mjs](../client/scripts/validate-animation-data.mjs)
- storyboard 直出：[client/scripts/render-frame-preview.mjs](../client/scripts/render-frame-preview.mjs)

## 数据流

```text
martial-art YAML animation.action
  -> CombatEventMsg.visual.actionId
  -> battleActionCatalog
  -> animationResolver
  -> ResolvedBattleTimeline
  -> PixiBattleStage
  -> PixiFrameActor swaps body/hair textures only when frameId changes
```

## 武学绑定

普通招式和主动技能都必须写固定 action：

```yaml
attack_moves:
  - id: trial_jian_thrust
    name: "试剑一刺"
    damage: 11
    animation:
      action: rig.sword.thrust_a
      tags: ["sword", "stab"]
```

`action` 缺失、为空或不存在时，内容校验直接失败。系统没有 animation pool，也不随机挑动作。

## Action manifest

manifest schema 当前为 v3：

```json
{
  "id": "rig.fist.punch_a",
  "frameset": "raster-v1",
  "style": "fist",
  "frames": [
    { "frameId": "punch_windup", "holdMs": 280 },
    { "frameId": "punch_strike", "holdMs": 440 }
  ],
  "impactFrame": 1,
  "durationMs": 720,
  "actorMotion": "approach"
}
```

`rig.*` 是为兼容现有武学 YAML 和服务端事件保留的历史 action ID 前缀；运行时已经没有 rig、骨骼或 pose 数据。

约束：

- `frameset` 必须是 `raster-v1`。
- `frames` 至少一帧，每帧 `holdMs > 0`。
- `durationMs` 必须等于所有 `holdMs` 之和。
- `impactFrame` 必须落在 `frames` 范围内。
- 每个 `frameId` 必须同时存在 body 和 hair PNG。

## 素材结构

```text
client/src/assets/battle/actors/raster-v1/
  fist/
    body/<frameId>.png
    hair/<frameId>.png
  sword/
    body/<frameId>.png
    hair/<frameId>.png
```

每张文件都是 `256x192 RGBA PNG`，人物使用统一比例与脚底基线。

渲染顺序：

1. female profile 显示 hair frame；male profile不显示。
2. body frame 画在 hair 上方。
3. 敌方在容器层整体水平镜像。

因此男女身体像素完全相同，性别差异只有马尾层。

## 关键帧生成与处理

源生成记录保存在：

```text
harness/animation-qa/runs/raster-keyframes-v1/
```

制作流程：

1. 使用项目 v13 actor anchor 和内置 `image_gen` 生成 fist/sword source sheet。
2. 使用纯 `#00ff00` chroma 背景，不生成服装、脸、阴影、轨迹或 VFX。
3. 使用 `normalize_keyframes.py` 做机械去背、共享缩放、脚底对齐和 `256x192` 导出。
4. 在编辑后的 sheet 中让模型只添加 cyan ponytail，再由 `extract_hair_overlays.py` 机械提取、着色和对齐。
5. 最终 body/hair 镜像到 runtime asset 目录。

旧的 segmented-v12 分件、pose JSON、骨骼编辑器和 renderer 已删除。旧服装版 fist/sword sprites 也不再保留。

## 命中同步

`animationResolver` 根据 action 的 `impactFrame` 算出 `impactAtMs`。以下反馈使用同一个时间点：

- 目标 hit/dodge/parry 姿态开始播放。
- 目标容器反应。
- impact/parry VFX。
- 伤害、治疗、闪避和格挡飘字。

人物关键帧本身不插值。Pixi actor 只有在 `frameId` 改变时才切换纹理。

## 本地校验与预览

```bash
cd client
npm run validate:animations
npm run check
npm run build
```

校验覆盖 action、frame 时长、impactFrame、martial-art 固定绑定以及 body/hair 文件存在性。

输出高频 storyboard：

```bash
npm run render:frames -- \
  --action rig.fist.heavy_a \
  --profile female \
  --fps 16 \
  --out harness/tmp/raster-v1/heavy-female.png
```

多条 `render:frames` 命令建议分别执行，再逐张检查 action semantics、起手/最大姿态、性别层、裁切和命中节奏。
