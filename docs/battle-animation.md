# 剪影交锋小舞台

当前战斗表现采用 SVG 人物剪影、连续姿态插值、短促交锋和克制的镜头反馈。人物采用大圆头、无五官、圆润躯干与四肢的矢量轮廓，贴近原始小人素材；人物与招式特效不再使用 PNG 图集。背景仍沿用已生成的山水 WebP。此次 SVG 美术是用户明确要求的代码原生矢量方案。

## 运行时

```text
martial-art YAML animation.action (+ params)
  → CombatEventMsg.visual.actionId + durationMs + params，hits[] 逐段结果
  → battleActionCatalog / animationResolver
  → game.ts 战斗事件队列
  → BattleClock（唯一播放时钟）
      ├─ battleDirector：纯函数采样位移、受击、镜头、强度
      ├─ svgBattlePose：按姿势键硬切，接触键钉住拳脚剑尖
      ├─ battleVfx：把强度和 sprite/custom 条目展开成特效精灵列表
      ├─ impact callback（每段一次）：显示气血更新与声音
      └─ complete callback：下一次交锋 / 结算
  → SilhouetteBattleStage.svelte：SVG 人物与特效 + transform/opacity
```

旧 PixiBattleStage、PixiFrameActor、Pixi runtime 和 pixi.js 依赖已删除。GSAP 仍用于其他 UI 的资源条。当前舞台只有两个角色和少量纹理层，SVG 姿态可以直接采样、定位和检查，无需维护另一套动画时钟。

核心文件：

- `client/src/battle/battleClock.ts`：播放、暂停、倍速、定位、取消；每次 play 都重置游标。
- `client/src/battle/battleDirector.ts`：完全确定性的画面采样，不读取当前时间、不生成随机画面。
- `client/src/battle/animationResolver.ts`：解析服务器动作与时长；给攻击添加专属接近段，并同步顺延命中及所有演出标记；展开 hits、poseTrack、offsetTrack、staging 与 params。
- `client/src/battle/battleTiming.ts`：命中定格的时间扭曲、当前段、姿势键、受击段与身法位移的查询，所有采样器共用。
- `client/src/battle/stagingProfile.ts`：读取 `combat_presentation/staging.json` 预设并按 动作 → params → 单段 合并。
- `client/src/battle/svgPoseLibrary.ts`：读取并校验 `combat_presentation/svg-poses.json` 姿势库。
- `client/src/battle/battleVfx.ts`、`vfxRegistry.ts`、`customVfx.ts`：特效精灵采样、自定义采样器注册表和内置采样器。
- `client/src/components/SilhouetteBattleStage.svelte`：实际游戏和回放页共用的 SVG 舞台。
- `client/src/battle/battleApproach.ts`：接近段的换位曲线（姿态、时长和起伏来自 manifest 的 approach）。
- `client/src/battle/svgBattlePose.ts`：按姿势键在关键姿势间硬切、接近段姿态、接触点反解；复用同一时间线和 hit stop。
- `client/src/battle/PoseSheet.svelte`（`/pose-sheet.html`）：姿势总览，调姿势用。
- `client/src/components/SvgBattleActor.svelte`：圆头、躯干、四肢、马尾和兵器的矢量轮廓。
- `client/src/game.ts`：权威状态、显示气血、队列和结算。
- `client/src/battle/battleAudio.ts`：可选的轻量 Web Audio 打击/招架/调息反馈，默认关闭，用户点击后启用。

## Action manifest v5

动作按风格分文件放在 `resources/scripts/combat_actions/`（`sword.json`、`fist.json`、`effects.json`，每个文件都是 `{ schemaVersion, actions }`），可以继续按武学拆出新文件。Haskell 服务端读取同一目录，只取 id、durationMs、approach.durationMs 和 hits 的 share；其余字段都是表现数据。历史 `rig.*` 前缀继续兼容武学 YAML，不代表运行时还有骨骼系统。

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
- share：伤害/治疗的分配权重，默认平分。服务端按同样的累计取整规则拆分，每段各自判定闪避/招架，并在 `CombatEventMsg.hits` 里给出逐段结果；客户端逐段推进气血、飘字、结果字、墨爆、闪白和镜头回弹。收到不带 hits 的旧消息时，客户端按 share 自行拆分总数，各段共用顶层结果。
- poseTrack：姿势键之间一律硬切。`pin` 把该姿势的 hand/foot/blade 钉到接触点，可用 reach/contactY 单独覆盖（reach 通常保持与 choreography 一致，因为人物站位按它计算）。第一个键必须在 0ms，接近步法末段混入这个姿势。至少要有一个 pin 键。
- 省略 poseTrack 时由 keyPoses 展开成四个键：到位 prepare → launchAtMs contact（pin reachWith）→ 最后一段定格结束 finish → restAtMs idle。
- 所有采样器通过 `battleTiming.ts` 的 visualTimeAt 读取定格后的时间；BattleClock 每段触发一次回调（参数为段序号），拖动时间轴不会重复触发已提交的段。

`rig.fist.combo_a`（连环三捶）是多段示例，暂未绑定到任何武学招式，可在 battle-lab 单招回放里查看。

### 舞台参数、身法轨道与远程招式

导演层不再写死数值。`resources/scripts/combat_presentation/staging.json` 按 choreography.weight 提供 light/heavy/quiet 预设（可 `extends`），字段包括：

- force：出手分量（印章、墨爆大小、斩痕倾角）。
- camera：kick（沿攻击方向的冲击）、rebound（二次回弹比例）、lift（竖向震动比例）、zoom（接触推近）。
- shade / tilt / flash / fallMs：压暗、舞台倾角、受击闪白、余劲衰减时长。
- trail：leadMs / fadeMs，剑光起笔与收笔窗口。
- reactions.hit / dodge / parry：push（位移）、tilt、lift、leadMs（提前量）、onsetMs（闪/架的起势时长）、ghost（残影）、standoff（招架时兵刃停在多远）、pose（覆盖受击方姿势）。

动作可写 `staging` 覆盖任意子集，单段命中还可在 `hits[i].staging` 上再覆盖；合并顺序为 预设 → 动作 → 该段。例：排山掌把受击改为击飞：

```json
"hits": [{ "atMs": 230, "hitStopMs": 110, "staging": {
  "camera": { "kick": 16 }, "fallMs": 320,
  "reactions": { "hit": { "push": 120, "tilt": 26, "lift": 14, "pose": "knocked_back" } }
} }]
```

多段命中里每段各自有结果和反应；连续同类反应视为一整段（闪避不会每段退回原位重来）。

`offsetTrack` 给出招者叠加根节点位移（x 朝向对手为正，y 向下为正，angle 为倾角），ease 取 cut（保持后跳变）、linear、out。接触键会反向补偿这段位移，所以跃起踢击的脚仍落在对手身上（见 `rig.fist.leap_kick_a` 飞燕踢）。

`actorMotion: "ranged"` 为远程招式：不接近、不位移，接触点仍按 contactY 计算，poseTrack 不需要 pin（见 `rig.sword.qi_wave_a` 剑气纵横）。

### 特效列表

舞台不再有固定的斩痕/墨爆/光环三个槽位，而是按 `battleVfx.ts` 的 `sampleVfx()` 输出的精灵列表逐个绘制。每个精灵有素材、位置、尺寸、缩放、旋转、朝向和透明度。

- 内置层：`trail`（斩痕，招架时换成招架墨环）、`impact`（墨爆）、`aura`/`heal`（光环）仍由导演层的 trail/burst/aura 强度驱动，manifest 里的条目只决定素材。
- `sprite`：自由特效，自带时间和运动：

```json
{ "kind": "sprite", "variant": "qi-crescent", "art": "slash", "anchor": "actor",
  "hit": 0, "offsetMs": -130, "durationMs": 150,
  "from": "actor.blade", "to": "contact", "size": 180, "scale": [0.55, 1],
  "fadeInMs": 30, "fadeOutMs": 40 }
```

  - 时间：写 `atMs` 按动作时钟；否则挂在 `hits[hit]` 上再加 `offsetMs`。随服务端时长缩放，读定格后的时间，所以会跟着命中定格停住。
  - 锚点：contact、actor、target（人物胸口）、center，或出招者的 actor.hand / actor.foot / actor.blade（剑尖），取当前姿势的实时位置。有 `to` 时在生命周期内从 from 移到 to。
  - 外观：size、scale [起, 止]、rotate、spin（生命周期内追加旋转）、opacity、fadeInMs/fadeOutMs；素材随攻击方向镜像，`mirror: true` 反过来镜像（剑气用它让弧形凸面朝前飞）。
  - `results`：只在该段结果属于列表时出现，例如只在命中时显示。
- `custom`：引用 `registerCustomVfx(name, sampler)` 注册的纯函数采样器，给写不进数据的特效用（见下一节）。

### 代码采样器（逃生口）

数据表达不了的效果（按参数生成多笔、程序化的轨迹等）写成纯函数，注册到 `client/src/battle/customVfx.ts`：

```ts
registerCustomVfx("blade_fan", ({ vfx, progress, direction, anchor, reduced }) => [
  /* 返回 VfxSprite[]：key、art、x、y、size、scale、rotate、flip、opacity */
]);
```

采样器拿到的是 timeline、当前场景采样、定格后的时间、0→1 的生命进度、攻击方向、锚点解析函数和减弱动态开关，只能根据这些算出精灵，不能自带时钟或随机数，这样暂停、慢放、拖动时间轴和截图测试都照常工作。manifest 用 `{ "kind": "custom", "effect": "blade_fan", "params": { … } }` 引用，时间、锚点、results 过滤与 sprite 相同；catalog 加载时会检查 effect 名已注册。内置的 `blade_fan`（剑网：以锚点为心扇形展开的多笔斩痕，params 为 count、spread、stagger）用于 `rig.sword.sword_net_a` 天罗剑网。

### 新增一个招式的流程

1. 姿势：在 `combat_presentation/svg-poses.json` 里用 extends/blend/set 加关键姿势（预备、接触、余劲，必要时加过渡姿势）。
2. 动作：在 `combat_actions/` 的某个文件里加 action。单段招式写 keyPoses 即可；多段写 hits + poseTrack（每段一个 pin 键）；需要跃起、后撤写 offsetTrack；远程用 `actorMotion: "ranged"`；手感用 staging 或 `hits[i].staging` 调；特效用 vfx 的 sprite / custom。
3. 绑定：武学 YAML 的 `animation.action` 指向它；只是换名字、换素材、调力度的变体，用 `animation.params`，不必新建动作。
4. 校验：`npm run validate:animations`（字段、姿势、素材、段序、剑光收笔时间）和 `npm run test:battle`；服务端 `stack test` 会检查所有招式引用的动作都存在。
5. 观感：`npm run dev` 打开 `/battle-lab.html` 单招回放，用暂停和拖动逐段检查接触、定格和特效。`choreography` 的定义：

- launchAtMs：从反向蓄势进入快速发力。
- hitStopMs：命中之后同时保持人物、镜头、轨迹与飘字位移的时长。
- recoverAtMs：开始平滑收回位移。
- restAtMs：回到对峙站位，剩余时间用于读结果。
- reach：动作的接触距离，以原始 256×192 素材坐标为单位。
- contactY：接触点相对原始素材顶端的高度；统一脚底基线是 176。
- weight：light、heavy、quiet，控制克制的镜头反馈和后坐力度。

标记满足 `0 ≤ launch ≤ impact ≤ impact + hitStop ≤ recover ≤ rest ≤ duration`。当前恢复“大开大合”版的原始攻击时长和时间标记，同时保留独立的远距离前倾冲刺。总时长为冲刺时长 + 服务端动作时长；launch、impact、recover、rest 整体顺延，hitStop 只随服务端时长缩放。没有额外的攻击分段加速。

## 交锋规则

- 节奏参照一款手游的录屏（2026-09 逐帧分析）：每招只用三四张关键姿势，每张定住 100–330ms；流畅感来自一直在动的东西——整个人的位移和特效——而不是四肢补间。
- 双方根位置相距 460 个素材像素（`SIDE_HOME = 230`），镜头缩到 0.62，冲刺距离才读得出来。
- 一次攻击：冲刺（一张冲刺画，整个人先快后慢地滑到距目标 reach 处，离地一点，影子留在地上）→ 蓄势定住 → 出手一帧到位并保持到特效散去 → 换后撤姿势滑回原位 → 站定。单招约 0.7–0.8s，加上 0.26–0.28s 冲刺。
- 接近、起手、接触和收势共用一条时间线；冲刺用 `dashIn`、后撤用 `dashOut`（`battleApproach.ts`），都是连续位移。
- hit 在接触时进入受击姿态；dodge 提前侧闪并留下短暂残影；parry 提前架势，接触时出现防守笔触，后退幅度很小。
- `svgAttackTrail.ts` 回采实际剑尖/拳脚轨迹，以细剑光和淡残影表现发力。火花和防守反馈仍围绕接触点组织。普通招式镜头缩放约 1.5%，重招约 3.5%。
- 相同 action 连续出现时依然是不同播放实例，不使用资源缓存键来决定是否重新开始。
- 后续队列达到三条时，只压缩站定读字尾段，保留蓄势、发力、命中和收势标记。
- 气血快照保留为权威状态；BattlePanel 的显示气血在 impact callback 时推进，队列清空后对齐快照。
- 结算排在最终命中之后；胜负演出完成后才退出战斗面板。
- 主游戏切到后台时清掉陈旧演出，保留战斗事实与日志并处理结算。后台新事件不再排成长时间回放，返回后展示最新状态。
- 减弱动态模式保留姿势、数字和轻量色彩反馈，关闭突进、震动、缩放、轨迹、残影和文字位移。

## 专属接近步法

每次攻击以一次冲刺完成接近。`approach.pose` 是整段冲刺保持的那一张画，`approach.durationMs`（当前 260/280ms）是冲刺时长，`lift` 是离地高度。`actor.actionDelayMs` 标记到位时间；接近期间 phase 为 approach，人物帧标记为冲刺姿势 ID（如 `approach_raised_step`）。攻击帧读取扣除接近段后的局部时间。积压队列仍仅压缩末尾静止读字时间。调息、持续效果和结算不接近。减弱动态模式隐藏位移，保留相同伤害时机。

冲到对手面前的招式（approach/lunge/drive），收招时一律换成 `keyPoses.retreat`（默认 `retreat`）滑回原位；有 poseTrack 的招式，收招标记之后的键会被后撤姿势取代。

## SVG 姿态与素材

关键姿势表是数据：`resources/scripts/combat_presentation/svg-poses.json`（schema 2，不放进 combat_actions，那个目录会被服务端逐个解析）。骨长是常量（`bones`：躯干、颈、头半径、上臂、前臂、大腿、小腿），姿势只给参数：

- `hip` 髋的位置；`torso`、`head` 躯干与头的朝向（度，0 朝前、-90 朝上、90 朝下）。头从颈根长出，肩关节在颈根下 8，手臂从胸口上沿长出来。
- `arm`、`backArm`：上臂、前臂的绝对朝向。手肘只能往前弯（前臂角度比上臂小）。
- `foot`、`backFoot`：双脚落点（y 接近 0 即踩地：脚踝抬高 5、脚掌平贴地面朝前；离地的脚顺着小腿指出去），腿用两段反解求膝盖；`knees` 为膝盖弯向，侧视图里两条腿都用 1（朝前）。脚够不着时髋自动下沉，所以宽弓步自然就蹲低了；双脚间距要控制在腿长能够到的范围（约 130）。
- `blade` 剑的朝向，`tassel` 剑穗方向点。

每个姿势从 base 加骨架差异（rigs.fist/sword）出发，或 `extends` 另一个姿势，或 `blend` 两个姿势（角度走最短弧），再用 `set` 覆盖参数，`rigs.sword` 单独覆盖剑手。接触键用反解把拳、脚送到接触点；剑招保持手臂不动，转剑让剑尖落点，身体只做横向微调。骨长因此在任何姿势和混合里都不变。

`npm run dev` 后打开 `/pose-sheet.html` 可以看到全部姿势按拳、剑两种骨架排成的总览，调姿势时用它逐个检查比例、关节弯向和落脚。`svgPoseLibrary.ts` 在加载时解析全部姿势、检查未知关节与循环引用；catalog 与 `validate:animations` 校验 manifest 引用的姿势和素材都存在。新增招式时，先在 svg-poses.json 加姿势，再在 manifest 引用，不需要改 TS。

`svgBattlePose.ts` 只负责按时间线在关键姿势之间切换。攻击从蓄势加速插值到最大动作，命中时保持，随后走完独立随势动作，再平滑收回。拳头、脚尖或剑尖由 manifest 的 reach/contactY 对齐接触位置。双方朝向仍由舞台镜像处理。弓步、反向展臂、举剑下劈、低起上挑与提膝侧踢形成不同的大开合轮廓；剑长为 72 个素材像素。

`sampleSvgPose` 使用 BattleClock 的 elapsed 与 choreography 标记，不启动 CSS/SMIL 独立动画，因此暂停、慢放、定位和 hit stop 同步。减弱动态模式使用离散姿态并关闭原有位移/震动。剪影是实心单色：四肢是锥形段（大腿粗于小腿、上臂粗于前臂），躯干是胸宽腰窄的一段，后侧手脚压暗一档以分前后。male/female profile 共用圆头身体，female 增加马尾：在后脑偏上束起，发根向后微微翘起一小截，再呈 S 形弧线垂到腰，发梢分叉成两个尖（形状参照早期生成的角色素材 `docs/assets/battle-animation/actor-style-reference-female.png`）。发根一截跟着头走，垂下部分受重力；舞台按人物滞后速度给出外力（冲刺向后扬、急停回甩、后撤向上扬、受击一震），摆动沿发束向下传。全身一个颜色，四肢从胸腔、骨盆两个体块里长出来。人物按由远到近拆成部件（头发、后腿、后臂、躯干与头、前腿、前臂与剑），每个部件先整体描一圈深色边再填色，所以部件内部没有接缝，前面的部件压到后面时留下一道遮挡线。肩、髋、发根这些自然连接处画完后会"缝合"：在连接点附近把相连几块的填色重铺一遍，盖掉那里的描边，只留外轮廓；真正的遮挡（前臂横过胸口、提膝挡住身体）不受影响。

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

当前为可运行的 SVG 风格试作，覆盖拳脚、剑术与无发饰/马尾两种轮廓；其他武器需要在 `SvgBattleActor.svelte` 增加矢量形状，并在姿势库 `rigs` 里加一个骨架差异。镜头只有冲击、回弹和推近三个参数，还没有独立的镜头关键帧轨道；受击方也只有位移、倾角、离地和换姿势，没有自己的多键轨道。攻击采用设计好的关节点插值，肢体允许适度伸缩，并非严格保持骨长的骨骼/IK 系统。舞台背景仍为位图。声音为轻量合成反馈；视觉 QA 不等同于全设备帧率或音质测量。
