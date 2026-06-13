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
- 可使用秘籍物品，使用后学习并准备武功。
- 角色养成系统：角色无等级，武功有等级和熟练度，基础功由具体武功升级带动。
- learn/practice/train/study/research/meditate 服务端动作。
- prepared/enabled 武功映射，主动招式从两者合并暴露。
- 战斗胜利奖励实战经验和潜能。
- 玩家 story/inventory/money/potential/combat_exp/HP/Qi/arts/prepared/enabled JSON 存档。
- 原生 Web 客户端。
- 武侠风房间 UI、方位图移动、NPC 弹框、背包使用按钮。
- 战斗专用剪影界面，敌我资源条、AP 平滑展示、战斗事件队列播放。
- UI 固定字段中/英文 i18n。
- 本地 dev 测试入口：`?test=1` 自动登录，`MUD_DEV_MODE=1` 下重置同名测试存档。
- 基础测试覆盖关键 gameplay flow。

## 当前内容

完整章节：

- `cold_rain_inn`：冷雨客栈。

主要 NPC：

- 冷雨掌柜。
- 青衣客。
- 纸伞客。
- 沉默木人。

主要物品：

- `伞骨银针`：纸伞客掉落证物。
- `听雨残谱`：青衣客结尾奖励，使用后学习 `cold_rain_secret`。

主要武功：

- `无名试刀`
- `冷雨短打`
- `听雨残谱`

## 已知缺口

- 没有真正数据库，只有 JSON 玩家存档。
- 角色位置尚未持久化。
- NPC 战斗锁、死亡状态和复活倒计时仍只在内存中维护，server 重启后回到 YAML 初始状态。
- dev 测试入口只能重置到默认初始状态，还没有一键跳转到指定剧情阶段或战斗场景的 seed 机制。
- 物品系统只有背包数量和 `learn_art` 使用效果，没有装备、消耗品、交易、掉落表。
- prepared/enabled 已有服务端命令，但还没有玩家主动切换 UI。
- 没有全局角色等级；基础属性成长尚未实现。
- 多人同房间广播、聊天可见范围、PVP 规则尚未完善；多人围攻同一个 NPC 尚未实现。
- NPC AI 仍很简单：普通攻击来自准备武功 attack_moves，主动招式 AI 尚未成体系。
- 普通攻击已有命中、闪避、招架和属性缩放；装备、防具、暴击和技能 hook 尚未接入。
- 剧情事件是线性匹配第一个可用事件，没有优先级、冷却、复杂变量或表达式系统。
- Lua 依赖在 package 中存在，但当前主线剧情没有使用 Lua。
- 测试是自定义 `Spec.hs` 主程序，没有 Hspec/Tasty 结构。

## 高优先级下一步

统一路线以 [MUD 调研综合结论与落地路线](./mud-design-synthesis.md) 为准。当前快照下，近期应按以下顺序收敛：

1. **收紧当前已落地系统**
   - 保存玩家当前位置。
   - 增加 `PlayerSave` 显式迁移函数。
   - 把已有 `enable/prepare` 命令接入 UI。
   - 增加 action registry 的最小版本，用于返回当前可用动作。

2. **DerivedStats + 装备实例**
   - 增加统一派生属性计算层。
   - 增加装备槽、装备实例、装备属性、穿脱命令/UI。
   - 将装备接入普通攻击、防御、耐久消耗和修理。

3. **房间运行态和掉落表**
   - 增加 `RoomObject`，支持钱、物品、尸体等临时对象。
   - 把剧情掉落从单个 action 扩展成可配置 drop table。
   - 增加 TTL、归属保护和拾取规则。

4. **busy、condition 和学习成本**
   - busy 机制，让学习、练功、研读、自研、打坐成为耗时动作。
   - 增加精神或行动资源。
   - 增加 `tickConditions`，统一战斗内外状态。

5. **门派/师承与循环 job**
   - 增加门派/师承规则和可学列表。
   - 增加师父教学上限、禁教规则和拜师条件。
   - 在剧情 quest 外新增可重复 job template。

6. **心法与数据库决策**
   - 在装备和派生属性稳定后引入心法/Technique 第二成长轴。
   - 如果装备实例、房间对象、job 和交易需求继续扩大，再决定是否从 JSON 存档迁移到 SQLite/Postgres。
