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
- dev 测试入口只能重置到默认初始状态，还没有一键跳转到指定剧情阶段或战斗场景的 seed 机制。
- 物品系统只有背包数量和 `learn_art` 使用效果，没有装备、消耗品、交易、掉落表。
- prepared/enabled 已有服务端命令，但还没有玩家主动切换 UI。
- 没有全局角色等级；基础属性成长尚未实现。
- 多人同房间广播、聊天可见范围、PVP 规则尚未完善。
- NPC AI 仍很简单：普通攻击来自准备武功 attack_moves，主动招式 AI 尚未成体系。
- 普通攻击已有命中、闪避、招架和属性缩放；装备、防具、暴击和技能 hook 尚未接入。
- 剧情事件是线性匹配第一个可用事件，没有优先级、冷却、复杂变量或表达式系统。
- Lua 依赖在 package 中存在，但当前主线剧情没有使用 Lua。
- 测试是自定义 `Spec.hs` 主程序，没有 Hspec/Tasty 结构。

## 高优先级下一步

1. **装备/物品系统成型**
   - 增加装备槽、装备属性、穿脱命令/UI。
   - 把剧情掉落从单个 action 扩展成可配置 drop table。

2. **武功管理**
   - 增加学习列表 UI。
   - 把已有 `enable/prepare` 命令接入 UI。
   - 区分“秘籍物品”、“已学武功”、“已准备武功”的 UI 展示。

3. **剧情脚本能力补强**
   - 增加 event priority 或明确排序策略。
   - 增加 action 前后是否刷新 view/inventory/quest 的显式策略。
   - 增加更丰富的 condition，例如 has_item、knows_art、money_at_least。

4. **角色成长**
   - busy 机制，让学习、练功、研读、自研、打坐成为耗时动作。
   - 基础属性成长或装备属性加成。
   - 门派/师承规则和可学列表。

5. **持久化范围决策**
   - 至少补充保存位置。
   - 决定 JSON 存档是否继续够用，还是引入 SQLite/Postgres。

6. **客户端交互完善**
   - 背包详情弹框。
   - 武功详情和切换。
   - 战斗日志/剧情日志更清晰分层。
   - 移动端布局继续打磨。
