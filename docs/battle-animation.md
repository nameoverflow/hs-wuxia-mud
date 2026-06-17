# 半回合制战斗动画系统

本文记录当前 Web 客户端战斗动画系统的实现边界。战斗事实由 server 发送结构化事件，client 通过动画 catalog 和 resolver 选择表现。

目标不是做完整 3D 或骨骼动画引擎，而是为当前一对一、AP 驱动、半回合制的文字 MUD 提供一个轻量、数据驱动、可扩展的 2D 剪影动画系统。

核心原则：

- server 决定战斗事实：谁出手、用什么招、命中/闪避/招架、伤害、治疗、结算。
- 内容 YAML 决定动画语义：动作池、指定动作、动作 tag。
- client 决定表现播放：从 catalog 中选 clip、motion、reaction、VFX 和飘字。
- UI 组件渲染 resolved timeline。
- 素材使用统一画布、统一比例和统一脚底基线。

## 当前源码入口

- 服务端视觉 hint 类型：[src/Game/Entity.hs](../src/Game/Entity.hs)
- 服务端战斗事件协议：[src/Game/Message.hs](../src/Game/Message.hs)
- 服务端事件产生：[src/Game/Combat.hs](../src/Game/Combat.hs)
- 客户端协议类型：[client/src/protocol.ts](../client/src/protocol.ts)
- 动画 catalog：[client/src/battle/animationCatalog.ts](../client/src/battle/animationCatalog.ts)
- timeline resolver：[client/src/battle/animationResolver.ts](../client/src/battle/animationResolver.ts)
- timeline 类型：[client/src/battle/animationTypes.ts](../client/src/battle/animationTypes.ts)
- 战斗面板渲染：[client/src/components/BattlePanel.svelte](../client/src/components/BattlePanel.svelte)
- 通用 motion / reaction / VFX CSS：[client/src/styles.css](../client/src/styles.css)
- 当前素材：[client/src/assets/battle/](../client/src/assets/battle)

## 骨骼绑定基础系统

骨骼系统当前是独立基础设施，不接入正式游戏 client UI。它用于先验证骨骼层级、锚点、绑定、姿势编辑和 Canvas 渲染。

源码入口：

- 骨骼类型：[client/src/battle/skeletal/types.ts](../client/src/battle/skeletal/types.ts)
- 2D transform 数学：[client/src/battle/skeletal/math.ts](../client/src/battle/skeletal/math.ts)
- pose/runtime 解析：[client/src/battle/skeletal/runtime.ts](../client/src/battle/skeletal/runtime.ts)
- Canvas renderer：[client/src/battle/skeletal/renderer.ts](../client/src/battle/skeletal/renderer.ts)
- battle action 到 rig 条目映射：[client/src/battle/skeletal/catalog.ts](../client/src/battle/skeletal/catalog.ts)
- segmented v12 动作 pose 数据：[client/src/battle/skeletal/data/segmented-v12-poses.json](../client/src/battle/skeletal/data/segmented-v12-poses.json)
- 独立开发工具入口：[client/animation-rig.html](../client/animation-rig.html)
- 工具组件：[client/src/tools/animation-rig/AnimationRigTool.svelte](../client/src/tools/animation-rig/AnimationRigTool.svelte)

运行工具：

```bash
cd client
npm run dev
# 打开 http://127.0.0.1:8080/animation-rig.html
```

工具页保存方式：

- `Save Local`：只保存当前浏览器的 localStorage，用于临时试 pose。
- `Save Project Poses`：仅在 `npm run dev` 的 Vite dev server 下可用，会把当前 segmented v12 pose edits 合并写回 `client/src/battle/skeletal/data/segmented-v12-poses.json`，刷新、构建和 PNG 直出都会加载保存后的动作。
- 当前 project save 只覆盖动作 pose 的 bones/pose anchors；绑定定义、全局锚点和部件资源仍在 `catalog.ts` 与素材文件中维护。

### Rig PNG 直出审查

如果只想审查当前 rig 渲染效果，不需要打开浏览器截图；用 `render:rig` 可以直接把 Canvas renderer 输出成 PNG。这个脚本复用同一套 `SkeletalCanvasRenderer`、catalog 和 runtime，因此适合快速看黑底带标注版、黑底 clean 版、透明 clean 版，以及多动作关键帧对比。

基础命令：

```bash
cd client
npm run render:rig -- --mode review --profile female --style sword --actions thrust,chop,rising_cut --width 720 --height 540 --out harness/tmp/rig-review-female-sword-attacks-xlarge.png
npm run render:rig -- --mode review --profile female --style sword --actions dodge,parry,hurt --width 720 --height 540 --out harness/tmp/rig-review-female-sword-reactions-xlarge.png
```

常用参数：

- `--mode debug|clean|review`：`debug` 单张黑底带骨骼/锚点/绑定；`clean` 单张透明 clean；`review` 一张图里并排输出黑底带标注、黑底 clean、透明 clean 三列。
- `--profile female|male`、`--style fist|sword`、`--tag v12`：按角色和动作风格筛选 catalog entry。
- `--actions thrust,chop,rising_cut`：按 action/tag 选多行动作，适合一次审查 2 到 3 行；动作太多会缩小到难以看清。
- `--entry segmented.v12.sword.thrust.female` 或 `--entries <id,id>`：需要精确指定 entry 时使用。
- `--poses <id,id>`：对同一个 entry 输出多个 pose。
- `--width 720 --height 540`：在 `review` 模式下是每个小面板的尺寸，不是整张图尺寸；看细节时优先放大面板或减少动作行数。
- `--fit actor|stage`：默认 `actor` 只按人物 bbox 自动缩放；`stage` 保留完整 rig stage。
- `--zoom`、`--pan-x`、`--pan-y`：手动覆盖自动 fit，用来检查局部拼接或裁切问题。
- `--out harness/tmp/...png`：临时审查图默认放在 `harness/tmp/`，需要长期保留的参考图再移动到 `harness/animation-qa/runs/`。

建议串行运行多条 `render:rig` 命令。并行运行时 Vite SSR 仍可能打印 HMR 端口占用警告，但 PNG 通常可以正常输出。

当前能力：

- 从现有 `battleActions` 自动列出所有 action/profile 组合。
- 每个条目套用标准 humanoid rig，包含 root、躯干、头、双臂、双腿、剑、impact 等骨骼/锚点。
- 新增 `segmented.v12` 右侧面短头身粗分件 actor rig，使用用户提供的拆分部件图做机械切片，并使用用户提供的骨骼节点图提取 source keypoint 做绑定。
- `segmented.v12` 采用 limb proximal extension 策略：肩/胯融合段不是独立分件，而是四肢靠近身体那一端自带的延长根部；渲染时这些根部藏到头身下面，由躯干和头部遮住拼接缝。
- `segmented.v12` 提供 `bind`、`idle`、`windup`、`strike`、`recover`、`guard`、`hurt`、`dodge` 离散关键帧；另有 `segmented.v12.sword.*` 条目用于预览中国剑术风格的刺、劈、撩、受击、闪避、格挡关键帧。工具默认停在当前关键帧，Play Preview 只用于临时补间预览。
- `segmented.v12` 的 `bind` 和 `idle` 现在是 anchor 参考图基准姿态：所有部件共享同一个 source scale，并按参考图 mask bbox / baseline 反推骨骼长度、keypoint 距离和默认节点位置。
- renderer 支持 source frame、debug skin、骨骼、锚点、binding link、label 和四肢/马尾 weighted mesh skinning 形变分层开关。
- `Pose` 模式可拖动带 `handleBoneId` 的锚点来旋转对应骨骼。
- `Pose` 模式也可拖动四肢 deform keypoint（elbow、wrist、knee、ankle），这些调整按当前关键帧保存；`Lock lengths` 默认打开，拖动四肢关键点时用两段 IK 保持骨段长度，避免手动编辑时把四肢拉长或压短。
- `Anchor` 模式可拖动关节/挂点，或在 inspector 中修改 anchor 所属 bone、局部 X/Y、拖动时控制的 bone。
- `Bind` 模式可拖动绑定点，或在 inspector 中修改 anchor、offset、rotation、scale、opacity。
- Canvas 视口支持滚轮/触摸板双指平移，macOS 触摸板 pinch 或 `Cmd/Ctrl`+滚轮会以指针位置为中心缩放。
- 调整结果可导出 JSON，也可保存到浏览器 localStorage 做本地迭代；导出包含 per-pose `poses`、全局 `anchors` 和全局 `bindings`。

当前约束：

- 大多数正式战斗 PNG 仍是完整帧图；`source.frame` 只是对照 binding，不代表最终骨骼切片资产。
- `segmented.v12` 当前四肢和马尾使用五点 weighted mesh skinning：renderer 生成高密度三角网格，每个 source 网格点按到多段 source keypoint 曲线的距离连续混合权重，再映射到 target keypoint 曲线。相比旧的最近中心线投影，这会保留马尾曲线、尖端和肢体轮廓，避免弯曲后出现方形端面或硬分区。
- 手臂 keypoint 是 hidden root、shoulder、elbow、wrist、hand；腿部 keypoint 是 hidden root、hip、knee、ankle、foot。它不再用半径生成新的圆管外形，所以手脚末端和部件粗细由源图 alpha 决定；当像素采样失败时才回退到 ribbon mesh。它已经支持从弯曲到伸直的关键点形变，但还不是完整的手工权重刷编辑器。
- 女版马尾使用同一套 weighted mesh skinning，`ponytailRoot` 固定到 head，`ponytailBase` 是靠头的固定基座，`ponytailMid`、`ponytailLower`、`ponytailTip` 作为后段链条参与摆动；马尾使用更密的网格和更宽的曲线影响半径。
- 剑术关键帧参考记录在 [harness/animation-qa/references/chinese-jian-keyframe-notes.md](../harness/animation-qa/references/chinese-jian-keyframe-notes.md)。当前剑是挂在 `sword` bone 上的 tool prop line，方便先调动作，不作为最终剑器美术。
- debug skin 是工具层预览，不作为最终游戏美术。
- 正式战斗面板仍走现有 sprite/CSS timeline；骨骼渲染接入需要后续单独切换 `ActorVisual` 和 `BattlePanel` 渲染路径。

## 总体架构

```text
Content YAML
  -> AttackMove / ActiveSkill animation
  -> Combat server
  -> CombatEventMsg
  -> client event queue
  -> animationResolver
  -> ResolvedBattleTimeline
  -> BattlePanel + CSS primitives
```

各层职责：

| 层 | 责任 | 交付物 |
| --- | --- | --- |
| Combat server | 结算战斗事实，随事件发送 `CombatVisualHint` | `CombatEventMsg` |
| Content YAML | 为普通招式和主动招式声明动画池、动作、tag | `animation` 字段 |
| Animation catalog | 定义 clip、动作池、动作时长、motion、target reaction、VFX | action / pool / clip 表 |
| Resolver | 从 `CombatEvent` 和 catalog 选择 `ResolvedBattleTimeline` | actor、target、VFX、飘字状态 |
| BattlePanel | 渲染 actor、target、VFX、飘字、结算层 | 舞台 DOM |
| CSS | 提供通用 motion、reaction、VFX primitive | 可复用动画 primitive |

## 服务端协议

普通攻击、主动技能、DoT/HoT tick 统一使用：

```json
{
  "tag": "CombatEventMsg",
  "contents": {
    "kind": "normal",
    "actorName": "无名客",
    "targetName": "沉默木人",
    "message": { "kind": "script", "text": "以拳试人" },
    "damage": 16,
    "heal": null,
    "result": "hit",
    "visual": {
      "pool": "weapon.fist.basic",
      "action": null,
      "tags": ["fist", "strike"]
    }
  }
}
```

`kind` 当前取值：

- `normal`：自动普通攻击。
- `active_skill`：玩家主动招式。
- `effect_tick`：DoT/HoT tick。

`result` 当前取值：

- `hit`
- `dodge`
- `parry`
- `effect`

结算仍使用 `CombatSettlementMsg`。它不是出手事实事件，client 会把它排在动画队列末尾，作为胜负短动画播放后再关闭战斗面板。

## CombatVisualHint

`CombatVisualHint` 定义在 [src/Game/Entity.hs](../src/Game/Entity.hs)，JSON 形态：

```json
{
  "pool": "weapon.sword.basic",
  "action": "skill.self_focus.guard",
  "tags": ["sword", "slash", "heavy"]
}
```

字段语义：

- `pool`：动作池。必填。resolver 从该池选择动作。
- `action`：指定动作。可选。需要固定表现的主动技能可直接指定。
- `tags`：动作语义标签。可选。用于在动作池内选择更合适的动作。

`CombatVisualHint` 保持语义层级，只包含动作池、指定动作和标签。图片路径、CSS primitive、时长和 VFX 组合由 client catalog 定义。

## YAML 内容格式

每个 `attack_moves` 条目必须写 `animation`：

```yaml
attack_moves:
  - id: shadow_spine
    name: "影里一刺"
    desc: "影子先动，人后动。"
    msg: "从影子里递出一刺"
    unlock_level: 3
    damage: 16
    animation:
      pool: "weapon.sword.basic"
      tags: ["sword", "stab"]
```

每个 `active_skills` 条目也必须写 `animation`：

```yaml
active_skills:
  - id: healing_palm
    name: "回灯掌"
    msg: "一掌按下，气息像残灯又亮"
    target: Self
    cost: 50
    ap_req: 40
    heal: 40
    animation:
      pool: "skill.self_focus"
      action: "skill.self_focus.palm"
      tags: ["heal", "self"]
    effect:
      self: []
      target: []
```

当前已使用的 pool：

- `weapon.sword.basic`
- `weapon.fist.basic`
- `skill.sword_focus`
- `skill.fist_focus`
- `skill.self_focus`
- `effect.tick`

## 客户端 Catalog

当前 catalog 位于 [client/src/battle/animationCatalog.ts](../client/src/battle/animationCatalog.ts)。

它包含：

- `spriteClips`：clip id 到不同 visual profile 的 PNG 映射。
- `battleActions`：动作 id 到 clip、tags、duration、motion、reaction、VFX 的映射。
- `actionVariants`：通用 action 到 style/profile 专用 action 的映射。
- `actionPools`：pool id 到基础候选、style 覆盖、profile 覆盖和 style+profile 覆盖的映射。
- `reactionClips`：hit/dodge/parry/effect 到反馈 clip 的映射。

当前角色 clip：

- `actor.sword.idle`
- `actor.sword.stab_a`
- `actor.sword.slash_a`
- `actor.sword.uppercut_a`
- `actor.sword.guard`
- `actor.fist.idle`
- `actor.fist.punch`
- `actor.fist.heavy`
- `actor.fist.kick`
- `actor.fist.guard`
- `actor.fist.healing_palm`
- `actor.common.hurt`
- `actor.common.dodge`
- `actor.common.parry`

每个 clip 当前都有 `male` 和 `female` 两套 PNG。角色性别来自 battle snapshot 中的 `combatantSnapshotGender`；`female` 使用女性 profile，其余值使用男性 profile。

当前通用目标反馈：

- `hit` -> `actor.common.hurt`
- `dodge` -> `actor.common.dodge`
- `parry` -> `actor.common.parry`
- `effect` -> idle

当前动作分三类：

- 基础通用动作：`sword.stab_a`、`sword.slash_a`、`sword.uppercut_a`、`fist.punch`、`fist.heavy`、`fist.kick`。
- profile 专用动作：例如 `sword.male.slash_drive`、`sword.female.stab_lunge`、`fist.male.heavy_drive`、`fist.female.kick_lunge`。
- 技能和效果动作：例如 `skill.sword_focus.male_guard`、`skill.fist_focus.female_palm`、`effect.dot`、`effect.hot`。

profile 专用动作可以复用同一个 clip，但有不同的 duration、motion、tags、VFX 组合。这样可以表达“同一个武功池在不同角色 profile 下使用不同动作逻辑”，而不是只换图片。

## Resolver 规则

当前 resolver 位于 [client/src/battle/animationResolver.ts](../client/src/battle/animationResolver.ts)。

选择规则：

1. 根据 actor side 读取 visual profile 和 combat style。
2. 如果 `visual.action` 存在，先通过 `actionVariants` 转成 style/profile 专用 action；如果专用 action 不存在，回退到原 action。
3. 如果没有固定 action，读取 `visual.pool` 对应动作池。
4. 动作池按 `actions -> styles -> profiles -> styleProfiles` 顺序合成候选；每层可以 append 或 replace。
5. 候选可以带 `weight` 和额外 tags。
6. 用 `visual.tags` 对候选 action tags + 候选 tags 打分。
7. 只在最高分候选中按 weight 随机选择。
8. 如果 pool 不存在或为空，回退到 actor combat style 对应的基础池；最后回退到该 style 的默认 action。

当前池内随机使用 `Math.random()`。这足够满足实时表现；如果以后要做确定性回放，可改为基于 battle/event/move/skill id 的 seeded random。

resolver 输出 `ResolvedBattleTimeline`：

```ts
interface ResolvedBattleTimeline {
  id: number;
  kind: "normal" | "active_skill" | "effect_tick" | "settlement";
  actorSide: "player" | "enemy";
  targetSide: "player" | "enemy";
  durationMs: number;
  actor: { side: BattleSide; sprite: string; visual: ActorVisual; motion: ActorMotion };
  target: { side: BattleSide; sprite: string; visual: ActorVisual; reaction: TargetReaction };
  result: CombatResult;
  damage: number | null;
  heal: number | null;
  floatText: string;
  text: string;
  vfx: TimelineVfx[];
}
```

## 半回合播放模型

server tick 可能在同一个响应批次中产生双方行动。client 不把它们合并，而是按消息顺序进入动画队列：

```text
AttackMsg
BattleStateMsg(ap=...)
CombatEventMsg(player -> enemy)
CombatEventMsg(enemy -> player)
BattleStateMsg(ap=...)
CombatSettlementMsg
```

播放策略：

1. `AttackMsg` 打开战斗面板，不播放出手动画。
2. `BattleStateMsg` 立即更新 HP/Qi/AP 目标值，AP 用前端插值平滑展示。
3. 每条 `CombatEventMsg` 解析为一个 `ResolvedBattleTimeline`。
4. timeline 按队列顺序播放。
5. timeline 开始播放时才把对应战斗文本追加到消息历史，避免日志先刷完、动画慢慢追。
6. `CombatSettlementMsg` 进入同一队列末尾，结算动画播完后关闭战斗面板并刷新 view/quests/inventory/arts。

当前默认时长由 catalog 控制：

- 基础普通动作：`720-900ms`
- profile 专用快速动作：`640-780ms`
- profile 专用重动作：`780-960ms`
- focus 技能：`640-780ms`
- effect tick：`560ms`
- settlement：`900ms`

server 侧普通行动节奏当前约为每名 combatant `2.0s` 一次行动。这个数值在 [src/Game/Combat.hs](../src/Game/Combat.hs) 的 `targetCombatantActionSeconds` 中定义，和前端动画时长是两个系统。

## BattlePanel 渲染边界

[BattlePanel.svelte](../client/src/components/BattlePanel.svelte) 当前只做：

- 读取 `state.battle.animation.activeTimeline`。
- 根据 timeline 渲染 player/enemy actor。
- 渲染 timeline 的 VFX layer。
- 渲染 damage/heal/dodge/parry 飘字。
- 渲染 settlement flash。
- 平滑展示敌我 AP。

素材映射集中在 `animationCatalog.ts`。动作选择集中在 `animationResolver.ts`。命中、闪避、招架、伤害和治疗结果来自 `CombatEventMsg`。

## CSS 边界

CSS 当前保留的是通用 primitive，而不是技能硬编码：

- actor motion：`motion-approach`、`motion-lunge`、`motion-drive`、`motion-focus`
- target reaction：`react-hit`、`react-dodge`、`react-parry`、`react-effect`
- VFX：`stage-vfx trail/impact/parry/aura/heal`
- VFX variant：`stab-line`、`slash-arc`、`uppercut-arc`、`hit-spark`、`parry-arc`、`guard-ring`、`heal-pulse`

## 素材规范

当前素材是 image gen 生成后切出的 PNG，不使用 SVG。

当前导出约定：

- 画布：`256x192`
- 统一源图缩放比例。
- 统一脚底基线。
- 透明背景。
- 不含文字、不含网格、不含边框。
- 敌方由 CSS/renderer 镜像，不单独出敌方素材。

当前文件：

```text
client/src/assets/battle/actors/common/male/*.png
client/src/assets/battle/actors/common/female/*.png
client/src/assets/battle/actors/sword/male/*.png
client/src/assets/battle/actors/sword/female/*.png
client/src/assets/battle/actors/fist/male/*.png
client/src/assets/battle/actors/fist/female/*.png
```

导出规则：

- 同一源图统一缩放后重排到统一画布。
- 每个动作可以有不同横向展开宽度。
- 下蹲、后仰、跳起可以导致 bbox 不同。
- 后续补 manifest 记录 pivot、weaponTip、impact anchor。

## AP 与动画同步

AP 条展示和出手动画是两个系统：

- `BattleStateMsg` 驱动 HP/Qi/AP 的最终数值。
- `CombatEventMsg` 驱动 half-turn 动画。

当前 UI 规则：

- AP 增长时插值到新值。
- AP 下降或归零时立即更新，避免资源条慢慢倒退。
- 动画不修改战斗数值，最终数值始终以 server snapshot 为准。

## 结果反馈

同一个 actor action 根据 `result` 组合不同 target reaction。

| result | target reaction | VFX | float text |
| --- | --- | --- | --- |
| `hit` | `hit` | `hit-spark` / `dot-spark` | `-damage` |
| `dodge` | `dodge` | 攻击轨迹保留，目标闪避 | `闪` |
| `parry` | `parry` | `parry-arc` | `架` |
| `effect` | `effect` | `guard-ring` / `heal-pulse` | `+heal` 或空 |

`CombatEvent.result` 是 server 事实，直接驱动目标反馈。

## 当前能力

- 普通攻击、主动技能、effect tick 统一通过 `CombatEventMsg` 进入动画系统。
- `AttackMove` / `ActiveSkill` 强制要求 `animation` 字段。
- 当前 martial arts YAML 都包含 animation hint。
- client 有独立 catalog、resolver 和 timeline types。
- client 能按 actor combat style 选择拳/剑基础动作池。
- client 能按 actor visual profile 选择不同动作池、固定 action 变体和 sprite 素材。
- pool candidate 支持 weight 和附加 tags，resolver 会在 tag 最高分候选中按 weight 随机。
- `BattlePanel.svelte` 只消费 resolved timeline。
- CSS 使用通用 motion、reaction 和 VFX primitive。
- 普通攻击、主动技能、effect tick、结算都进入动画队列。
- AP 展示平滑增长。
- server 行动节奏调整到约 2 秒一次行动。

## 后续扩展

近期最有价值的后续工作：

1. 把 `segmented.v12` 的锚点和默认 pose 从工具导出固化为可版本化 manifest。
2. 增加 catalog validator，检查 pool/action/clip/vfx 引用和图片存在性。
3. 增加本地 animation preview route，用于预览 pool/action/result 组合。
4. 为暗器、掌法、指法分别补独立基础素材池。
5. 为重点武功和主动技能增加专属 action 和 VFX。
6. 如果需要战斗回放，再把 resolver 随机改成 seeded random。

## QA 建议

视觉 QA 至少覆盖：

- 桌面视口和一个窄屏视口。
- hit、dodge、parry。
- 主动技能 damage 和 heal。
- 战斗结束结算。
- console 无相关 error/warn。
- 动作中 DOM 出现 `motion-*`、`react-*`、`stage-vfx`。

协议 QA 至少覆盖：

- `CombatEventMsg` 包含 `kind/result/damage/heal/visual`。
- 修改招式中文 `msg` 不改变动画选择。
- 缺失 `animation` 的 attack move / active skill 会在内容加载阶段失败。
