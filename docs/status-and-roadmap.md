# 当前状态与路线图

## 已实现

- Stack/Haskell server 启动。
- WebSocket 登录和动作循环。
- 基于 YAML 的地图、NPC、武功、状态、物品、剧情加载。
- 世界引用校验。
- 8 方向移动。
- 房间查看，包含可见 NPC 和出口 summary。
- NPC 对话和攻击交互。
- 一对一战斗。
- 共享 NPC encounter 运行态：开战锁定 NPC，防止第二名玩家同时攻击同一个 NPC。
- AP/Qi/cooldown/status requirement 主动招式系统。
- DoT/HoT/buff/debuff 基础状态。
- 战斗快照推送给 UI。
- 剧情事件系统：trigger、condition、线性 action 编排。
- 章节目标/任务列表。
- 奖励消息、背包消息。
- 剧情杀敌后隐藏 NPC。
- 剧情掉落物。
- 可使用秘籍物品，使用后学习武功；研读在秘籍等级带内推进熟练度。
- 长线养成（见 [角色养成系统设计](./character-progression.md)）：熟练度逐步累积；实战经验三次曲线卡上限；learn/practice/study/research/meditate 各有成本与来源上限；practice 不超过基础功、瓶颈时回流基础功；基础功按具体武功熟练度的一半累积；武学分入门/地域两阶并写真实门槛。
- 养成动作可带次数，进入 busy 队列，逐步结算，可中止。
- 每个类型一个启用武功槽，普通攻击、派生数值、主动招式都读它；`prepare` 是别名。
- 战斗胜利按双方武功档次差奖励实战经验和潜能，训练对象有奖励上限；剧情任务可奖励实战经验和潜能；潜能有软上限。
- 战斗外 HP/Qi/Jing 按派生速度恢复，客舍休息三倍；战败后重伤 5 分钟，恢复减半且不能主动开战。
- 玩家 story/inventory/money/potential/combat_exp/HP/Qi/Jing/profile/innate/arts/enabled JSON 存档。
- 原生 Web 客户端。
- 武侠风房间 UI、方位图移动、NPC 弹框（含学艺）、背包使用/研读按钮、武学面板（熟练度、练功、自研、启用、busy 停下）、打坐按钮。
- 战斗快照驱动的敌我资源条和主动招式 UI。
- 角色属性第一层模型：`gender`、数值容貌、`InnateAttrs`、`Jing`、`DerivedStats` 纯计算和角色面板展示。
- `DerivedStats` 已接入 AP 增长、普通攻击、主动招式、状态 modifier、战斗快照 HP/Qi 上限和战斗奖励估算。
- 数据驱动战斗剪影动画：`CombatEventMsg`、YAML `animation` hint、客户端 catalog/resolver、half-turn 队列。
- UI 固定字段中/英文 i18n。
- 本地 dev 测试入口：`?test=1` 自动登录，`MUD_DEV_MODE=1` 下重置同名测试存档。
- 基础测试覆盖关键 gameplay flow。

## 当前内容

完整章节：

- `weiyuan_bloody_case`：威远镖局旧案，少年留书出走。
- `first_steps_kaifeng`：开封落脚、武馆试招、第一份城内活计。
- `kaifeng_echoes`：无任务栏的城中风声（告示、说书人传闻、威远总局来人）。
- `caoban_dock_rules`：漕帮码头差事，得到第一个江湖名号“南渡客”。
- `three_city_leads`：玩家可见为“远行门路”，从洛阳、大理、太湖中选择第一次远行方向。
- `journey_luoyang` / `journey_dali` / `journey_taihu`：首次远行的路上事件，取得各城的门户信任。
- `city_households`：段氏护院总管、太湖白发医者认人。
- `north_road_patrol`：北郊递道失信。

主要 NPC：

- 序章：受伤老镖师陈伯、灰衣少年、黑衣人。
- 开封江湖：过江鳅杜三、盐帮打手、铁旗罗万川、说书人张一嘴、顾捕头。
- 路上：醉剑客、山道劫匪、水寨喽啰。
- `威远木桩` 只作测试战斗与传授的 fixture，不放在任何地图上。

主要物品：

- `水浸路单`：陈伯临终交出的镖单，供较晚剧情回响。
- `威远剑谱`：少年之父手抄的家传剑谱，使用后学习 `weiyuan_sword`。
- `樊楼房牌`、`开封工筹`、`同行帖`、`递铺木牌`。

主要武功：

- `无名试拳`
- `镖局短打`
- `威远剑法`
- 地域武学：`洛阳剑法`、`茶马护身拳`、`太湖调息`、`烟水步`，均需本地门户信任才能学。

## 已知缺口

- 没有真正数据库，只有 JSON 玩家存档。
- 角色位置已进入 `PlayerSave`；旧存档没有位置时仍使用默认出生点。
- NPC 战斗锁、死亡状态和复活倒计时仍只在内存中维护，server 重启后回到 YAML 初始状态。
- 完整游戏的 dev 测试入口仍只能重置到默认初始状态，没有指定剧情阶段的 seed 机制；客户端 battle lab 已有战斗 fixtures，但不等同于服务端剧情或存档 seed。
- 物品系统只有背包数量和 `learn_art` 使用效果，没有装备、消耗品、交易、掉落表。
- 没有全局角色等级；基础属性成长尚未实现。
- `DerivedStats` 的装备、心法细项 hook 已预留，但装备实例和心法实例尚未落地。
- 养成节奏的数值只经过公式估算和单元测试，还没有确定性的节奏模拟测试，也没有经过实际游玩验证。
- 师承只有一层（`teach_requires` flag），记名/入室和门派绝学尚未实现；没有可重复的江湖差事，实战经验和潜能主要来自战斗和一次性剧情。
- 重伤只能等时间恢复，还没有治伤药品。
- 多人同房间广播、聊天可见范围、PVP 规则尚未完善；多人围攻同一个 NPC 尚未实现。
- NPC AI 仍很简单：普通攻击来自启用武功 attack_moves，主动招式 AI 尚未成体系。
- 普通攻击已有命中、闪避、招架和属性缩放；装备、防具、暴击和技能 hook 尚未接入。
- 战斗剪影动画已有 catalog validator（`client/scripts/validate-animation-data.mjs`）、SVG 姿势库与舞台参数（`resources/scripts/combat_presentation/`）、预览页（`client/battle-lab.html`、`client/pose-sheet.html`）、逻辑/浏览器测试和录屏入口。武功/技能专属素材与视觉质量仍需按动作逐项验收；功能与视觉验收以相应测试和 QA 记录为准。
- 剧情事件是线性匹配第一个可用事件，没有优先级、冷却、复杂变量或表达式系统。
- Lua 依赖在 package 中存在，但当前主线剧情没有使用 Lua。
- 测试是自定义 `Spec.hs` 主程序，没有 Hspec/Tasty 结构。

## 高优先级下一步

统一路线以 [MUD 调研综合结论与落地路线](./mud-design-synthesis.md) 为准。当前快照下，近期应按以下顺序收敛：

1. **收紧当前已落地系统**
   - 保存玩家当前位置。
   - 增加 `PlayerSave` 显式迁移函数。
   - 增加 action registry 的最小版本，用于返回当前可用动作。

2. **装备实例接入 DerivedStats**
   - 增加装备槽、装备实例、装备属性、穿脱命令/UI。
   - 将装备接入普通攻击、防御、耐久消耗和修理。

3. **房间运行态和掉落表**
   - 增加 `RoomObject`，支持钱、物品、尸体等临时对象。
   - 把剧情掉落从单个 action 扩展成可配置 drop table。
   - 增加 TTL、归属保护和拾取规则。

4. **condition 和战斗内 busy**
   - 养成动作的 busy 队列已落地；战斗内 busy（出招后摇、控制）尚未实现。
   - 增加 `tickConditions`，统一战斗内外状态（重伤目前单独计时）。
   - 治伤药品。

5. **门派/师承与循环 job**
   - 增加门派/师承规则和可学列表。
   - 增加师父教学上限、禁教规则和拜师条件。
   - 在剧情 quest 外新增可重复 job template。

6. **心法与数据库决策**
   - 在装备和派生属性稳定后引入心法/Technique 第二成长轴。
   - 如果装备实例、房间对象、job 和交易需求继续扩大，再决定是否从 JSON 存档迁移到 SQLite/Postgres。
