# 剪影交锋小舞台

当前战斗表现采用 SVG 人物剪影、连续姿态插值、短促交锋和克制的镜头反馈。人物采用大圆头、无五官、圆润躯干与四肢的矢量轮廓，贴近原始小人素材；人物与招式特效不再使用 PNG 图集。背景仍沿用已生成的山水 WebP。此次 SVG 美术是用户明确要求的代码原生矢量方案。

## 运行时

```text
martial-art YAML animation.action
  → CombatEventMsg.visual.actionId + durationMs
  → battleActionCatalog / animationResolver
  → game.ts 战斗事件队列
  → BattleClock（唯一播放时钟）
      ├─ battleDirector：纯函数采样姿势、位移、命中、镜头、特效
      ├─ impact callback：显示气血更新与声音
      └─ complete callback：下一次交锋 / 结算
  → SilhouetteBattleStage.svelte：SVG 人物与特效 + transform/opacity
```

旧 PixiBattleStage、PixiFrameActor、Pixi runtime 和 pixi.js 依赖已删除。GSAP 仍用于其他 UI 的资源条。当前舞台只有两个角色和少量纹理层，SVG 姿态可以直接采样、定位和检查，无需维护另一套动画时钟。

核心文件：

- `client/src/battle/battleClock.ts`：播放、暂停、倍速、定位、取消；每次 play 都重置游标。
- `client/src/battle/battleDirector.ts`：完全确定性的画面采样，不读取当前时间、不生成随机画面。
- `client/src/battle/animationResolver.ts`：解析服务器动作与时长；给攻击添加专属接近段，并同步顺延命中及所有演出标记。
- `client/src/components/SilhouetteBattleStage.svelte`：实际游戏和回放页共用的 SVG 舞台。
- `client/src/battle/battleApproach.ts`：按 action ID 定义前倾冲刺姿态、时长和起伏。
- `client/src/battle/svgBattlePose.ts`：矢量姿态、连续插值、招式 reach 接触点匹配；复用同一时间线和 hit stop。
- `client/src/components/SvgBattleActor.svelte`：圆头、躯干、四肢、马尾和兵器的矢量轮廓。
- `client/src/game.ts`：权威状态、显示气血、队列和结算。
- `client/src/battle/battleAudio.ts`：可选的轻量 Web Audio 打击/招架/调息反馈，默认关闭，用户点击后启用。

## Action manifest v5

`resources/scripts/combat_actions/battle-actions.json` 仍与 Haskell 服务端共享 action ID 和 durationMs。历史 `rig.*` 前缀继续兼容武学 YAML，不代表运行时还有骨骼系统。

普通攻击包含：站定 → 蓄势 → 最大动作 → 收势 → 站定。每帧的 holdMs 与总 durationMs 一致；impactFrame 标记最大动作开始。例：

```json
{
  "id": "rig.fist.punch_a",
  "label": "直拳",
  "durationMs": 620,
  "impactFrame": 2,
  "frames": [
    { "frameId": "idle", "holdMs": 60 },
    { "frameId": "punch_windup", "holdMs": 140 },
    { "frameId": "punch_strike", "holdMs": 100 },
    { "frameId": "punch_windup", "holdMs": 160 },
    { "frameId": "idle", "holdMs": 160 }
  ],
  "choreography": {
    "launchAtMs": 120,
    "hitStopMs": 35,
    "recoverAtMs": 300,
    "restAtMs": 460,
    "reach": 72,
    "contactY": 110,
    "weight": "light"
  }
}
```

其余已有的 style、frameset、actorMotion、tags、targetReaction 字段仍须保留。v5 起，招式的外观全部由数据声明，TS 代码里不再按 actionId 或 frameId 字符串分支：

```json
"keyPoses": { "prepare": "punch_windup", "contact": "punch_strike", "finish": "punch_finish", "reachWith": "hand" },
"approach": { "pose": "approach_step_in", "durationMs": 150, "lift": 3 },
"vfx": [{ "kind": "trail", "variant": "stab-line", "anchor": "actor", "art": "thrust" }]
```

- keyPoses：SVG 采样器硬切的三个关键姿势（蓄势、接触、余劲），均为 `svg-poses.json` 的姿势 ID。contact 必须等于 impactFrame 的 frameId。reachWith 取 hand、foot、blade，决定哪个点对齐 reach/contactY，也决定攻击轨迹追踪哪个点。focus 动作只需 contact（持势姿势）。
- approach：接近步法的姿势、基准时长与离地高度；冲刺末段混入 keyPoses.prepare。
- vfx[].art：舞台素材键，对应 `stageAssets.ts` 的 stageArt（impact、slash、thrust、rising、parry、aura）。
- 受击方姿势取反应动作（rig.*.hurt/dodge/parry）第一帧的 frameId。

### 多段命中与姿势轨道

一招可以有多次接触。`hits` 与 `poseTrack` 都写在动作自身的时钟上（不含接近段，随服务端 durationMs 等比缩放）：

```json
"hits": [
  { "atMs": 150, "hitStopMs": 40, "share": 1 },
  { "atMs": 330, "hitStopMs": 40, "share": 1 },
  { "atMs": 560, "hitStopMs": 80, "share": 2 }
],
"poseTrack": [
  { "atMs": 0, "pose": "punch_windup" },
  { "atMs": 115, "pose": "punch_strike", "pin": "hand" },
  { "atMs": 190, "pose": "combo_recoil" },
  { "atMs": 285, "pose": "low_punch_strike", "pin": "hand", "contactY": 132 },
  { "atMs": 370, "pose": "kick_windup" },
  { "atMs": 505, "pose": "kick_strike", "pin": "foot", "contactY": 104 },
  { "atMs": 640, "pose": "kick_finish" },
  { "atMs": 760, "pose": "idle" }
]
```

- hits：每段一次定格。第一段必须落在 impactFrame 上；后一段不能落在前一段定格内；最后一段定格结束不晚于 recoverAtMs。省略时等于 `[{ atMs: impactFrame 起点, hitStopMs: choreography.hitStopMs }]`。
- share：伤害/治疗的分配权重，默认平分。客户端按累计取整拆分服务端给的总数，保证各段之和不变；气血显示、飘字、墨爆、闪白、镜头回弹都逐段触发。服务端目前仍只给一个结果，所以各段共用同一个 hit/dodge/parry。
- poseTrack：姿势键之间一律硬切。`pin` 把该姿势的 hand/foot/blade 钉到接触点，可用 reach/contactY 单独覆盖（reach 通常保持与 choreography 一致，因为人物站位按它计算）。第一个键必须在 0ms，接近步法末段混入这个姿势。至少要有一个 pin 键。
- 省略 poseTrack 时由 keyPoses 展开成四个键：到位 prepare → launchAtMs contact（pin reachWith）→ 最后一段定格结束 finish → restAtMs idle。
- 所有采样器通过 `battleTiming.ts` 的 visualTimeAt 读取定格后的时间；BattleClock 每段触发一次回调（参数为段序号），拖动时间轴不会重复触发已提交的段。

`rig.fist.combo_a`（连环三捶）是多段示例，暂未绑定到任何武学招式，可在 battle-lab 单招回放里查看。`choreography` 的定义：

- launchAtMs：从反向蓄势进入快速发力。
- hitStopMs：命中之后同时保持人物、镜头、轨迹与飘字位移的时长。
- recoverAtMs：开始平滑收回位移。
- restAtMs：回到对峙站位，剩余时间用于读结果。
- reach：动作的接触距离，以原始 256×192 素材坐标为单位。
- contactY：接触点相对原始素材顶端的高度；统一脚底基线是 176。
- weight：light、heavy、quiet，控制克制的镜头反馈和后坐力度。

标记满足 `0 ≤ launch ≤ impact ≤ impact + hitStop ≤ recover ≤ rest ≤ duration`。当前恢复“大开大合”版的原始攻击时长和时间标记，同时保留独立的远距离前倾冲刺。总时长为冲刺时长 + 服务端动作时长；launch、impact、recover、rest 整体顺延，hitStop 只随服务端时长缩放。没有额外的攻击分段加速。

## 交锋规则

- 双方在中心附近对峙，根位置相距 280 个素材像素。先原地前倾压缩，再单步加速冲到距目标 reach 的位置；到位承接蓄势姿态，再按原版节奏出招；攻击期间根位置保持不动，收势时配退步回到原位。
- 接近、起手、接触和收势共用一条时间线。恢复位移采用平滑曲线，不 set 回原点。
- hit 在接触时进入受击姿态；dodge 提前侧闪并留下短暂残影；parry 提前架势，接触时出现防守笔触，后退幅度很小。
- `svgAttackTrail.ts` 回采实际剑尖/拳脚轨迹，以细剑光和淡残影表现发力。火花和防守反馈仍围绕接触点组织。普通招式镜头缩放约 1.5%，重招约 3.5%。
- 相同 action 连续出现时依然是不同播放实例，不使用资源缓存键来决定是否重新开始。
- 后续队列达到三条时，只压缩站定读字尾段，保留蓄势、发力、命中和收势标记。
- 气血快照保留为权威状态；BattlePanel 的显示气血在 impact callback 时推进，队列清空后对齐快照。
- 结算排在最终命中之后；胜负演出完成后才退出战斗面板。
- 主游戏切到后台时清掉陈旧演出，保留战斗事实与日志并处理结算。后台新事件不再排成长时间回放，返回后展示最新状态。
- 减弱动态模式保留姿势、数字和轻量色彩反馈，关闭突进、震动、缩放、轨迹、残影和文字位移。

## 专属接近步法

每次攻击以一次前倾冲刺完成接近，然后承接“大开大合”版攻击。前 50% 时间在原地压身，后 50% 用加速曲线覆盖距离并急停。`actor.actionDelayMs` 标记到位时间；接近期间 phase 为 approach，人物帧标记为 approach.pose 的姿势 ID（如 `approach_raised_step`）。攻击帧读取扣除接近段后的局部时间。积压队列仍仅压缩末尾静止读字时间，保留接近与攻击标记。

| 招式 | 位移动作 | 基准接近时长 |
| --- | --- | --- |
| 直拳 | 收拳前倾冲刺 | 150ms |
| 沉劲掌 | 深压身蹬地爆冲 | 165ms |
| 横踢 | 低掠提膝冲刺 | 165ms |
| 一线穿云 | 收剑前倾直冲 | 140ms |
| 横江一扫 | 拖剑前倾冲刺 | 155ms |
| 迎风劈剑 | 负剑前倾冲刺 | 160ms |
| 挑灯式 | 低身拖剑爆冲 | 155ms |

各招有独立的前倾躯干、持械和蹬腿姿态，不使用交替迈步循环。冲刺末段衔接蓄力姿态，攻击恢复连续的大幅挥斩和独立随势动作；回位也采用单次撤身。调息、持续效果和结算不接近。减弱动态模式隐藏位移与连续步法，保留相同伤害时机。

攻击段恢复 manifest 的 600–820ms，冲刺段保持 140–165ms。保留宽站位、弓步与反向展臂、72px 剑身、实际剑尖轨迹和淡残影。撤回后续的攻击加速、关键姿态跳切和延长尾停；积压队列保留 45ms 尾段。

## SVG 姿态与素材

关键姿势表是数据：`resources/scripts/combat_poses/svg-poses.json`（不放进 combat_actions，那个目录会被服务端逐个解析）。每个姿势从 base 加骨架差异（rigs.fist/sword）出发，或 `extends` 另一个姿势，或 `blend` 两个姿势（from/to/amount），最后用 `set` 覆盖个别关节。`svgPoseLibrary.ts` 在加载时解析全部姿势、检查未知关节与循环引用；catalog 与 `validate:animations` 校验 manifest 引用的姿势和素材都存在。新增招式时，先在 svg-poses.json 加姿势，再在 manifest 引用，不需要改 TS。

`svgBattlePose.ts` 只负责按时间线在关键姿势之间切换。攻击从蓄势加速插值到最大动作，命中时保持，随后走完独立随势动作，再平滑收回。拳头、脚尖或剑尖由 manifest 的 reach/contactY 对齐接触位置。双方朝向仍由舞台镜像处理。弓步、反向展臂、举剑下劈、低起上挑与提膝侧踢形成不同的大开合轮廓；剑长为 72 个素材像素。

`sampleSvgPose` 使用 BattleClock 的 elapsed 与 choreography 标记，不启动 CSS/SMIL 独立动画，因此暂停、慢放、定位和 hit stop 同步。减弱动态模式使用离散姿态并关闭原有位移/震动。male/female profile 共用圆头身体，female 增加简洁的波浪马尾。

历史 action manifest 的 `frameset: raster-v1` 字段、帧 ID 和 PNG 原始资源暂时保留用于兼容与旧资源校验；运行时 ActorVisual.kind 为 svg，catalog 校验 SVG 姿态覆盖。旧图集不再由舞台引用或预加载，生产 bundle 不包含人物和特效图集。`pack:battle` 仅用于维护旧图集，修改 SVG 不需要重打包。

背景保留 `client/src/assets/battle/ink-stage-v1/backdrop.webp`。SVG 替换的设计与 QA 记录位于 `harness/animation-qa/runs/svg-silhouette-v1/`。

## Agent 制作和验收

```bash
cd client
npm run check
npm run test:battle
npm run test:battle:browser
npm run build
```

浏览器测试使用 Playwright。首次可运行 `npx playwright install chromium`；使用已安装 Chrome 时设置 `PLAYWRIGHT_CHANNEL=chrome`。

`npm run dev` 后打开 `/battle-lab.html`：

- 完整交锋经过真实游戏事件入口和队列，包含同招连击、招架、闪避、受伤、调息和结算。
- 可以单独选择动作、结果、出招方与剪影 profile。
- 暂停、倍速和拖动时间轴调用同一 BattleClock。拖动只定位画面，不重复提交伤害；继续播放才提交尚未经过的命中回调。
- `window.__battleLab` 仅在独立回放页存在，供自动录制和语义检查使用。主游戏没有这个测试接口。

录制和密集 storyboard：

```bash
cd client
PLAYWRIGHT_CHANNEL=chrome npm run record:battle
cd ..
bash .codex/skills/animation-visual-qa/scripts/storyboard-from-video.sh \
  harness/tmp/silhouette-stage-v2/recording/recording.webm \
  harness/tmp/silhouette-stage-v2/storyboards 3 16 8 320
```

`render:frames` 仍可用于单独查看角色帧顺序；它不能替代完整舞台回放。最终验收同时检查全速节奏、慢放与密集 storyboard，不以字段校验通过代替观感判断。

## 已知表达边界

当前为可运行的 SVG 风格试作，覆盖拳脚、剑术与无发饰/马尾两种轮廓；其他武器需要新增姿态和矢量形状。攻击采用设计好的关节点插值，肢体允许适度伸缩，并非严格保持骨长的骨骼/IK 系统。舞台背景仍为位图。声音为轻量合成反馈；视觉 QA 不等同于全设备帧率或音质测量。
