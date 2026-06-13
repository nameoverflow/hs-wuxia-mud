# 客户端 UI

客户端位于 [client/](../client)，是 Svelte + TypeScript + Vite WebSocket 客户端。

界面风格是简洁框线式 MUD 控制台：暗色底、直线分区、状态色资源条，不依赖场景图或角色图片。

## 当前职责

- 登录并连接 `ws://127.0.0.1:9160`。
- 渲染玩家气血、内力和状态；行动只在战斗相关 UI 中显示。
- 渲染当前房间、出口方位图、NPC 列表。
- 点击方位图节点直接移动。
- 点击 NPC 在人物面板内展开交互操作。
- 点击“交谈”后，`DialogueMsg` / `StoryMsg` 正文追加到消息历史。
- 显示任务、背包、奖励消息。
- 角色状态和背包合并在左侧同一面板中，通过 tab 切换。
- 背包中可使用物品显示“使用”按钮。
- 非战斗状态显示已学武功、基础功等级、解锁招式和训练按钮。
- 战斗中显示敌我状态、主动招式卡、冷却、前置状态需求。
- 战斗消息进入消息历史，战斗快照驱动敌我资源和主动招式可用性。
- 支持中/英文 UI 固定字段 i18n。
- 支持本地测试 URL 自动登录。

## 测试入口

正常入口仍然通过登录面板输入用户名并连接。

本地开发可以打开：

```text
http://127.0.0.1:8080/?test=1
```

client 会自动填入 `tester` 并连接。默认会把 `Login.password` 设置为 `__dev_reset`，请求 server 重置该测试玩家存档。server 必须用 `MUD_DEV_MODE=1` 启动，否则会返回 `dev_mode_required` 错误。

可选参数：

- `user=<name>`：指定测试用户名。
- `reset=0`：自动登录但不请求重置存档。

## UI i18n 边界

只对 UI 固定字段做 i18n，例如：

- 面板标题。
- 按钮。
- 状态名。
- 主动招式卡上的固定标签。
- 空列表占位。

不翻译：

- 剧情文本。
- NPC 名字。
- 房间名/描述。
- 物品名/描述。
- 武功名/描述。
- 招式名/描述。

这些内容都由 YAML 脚本控制。

## 房间方位图

server 返回 `ViewMsg` 时包含：

- 房间名、描述。
- 可见 NPC summary。
- 出口 summary：方向、目标房间 id、目标房间名、目标坐标。

client 根据出口方向布局节点，当前位置在中间，出口节点可点击。点击出口节点发送：

```json
{"go":"NorthEast"}
```

## NPC 弹框

`ViewMsg` 中每个 NPC 带 `actions`：

- `talk`
- `attack`
- `sparring`

client 点击 NPC 后打开弹框：

- 有 `talk` 时显示“交谈”。
- 有 `attack` 时显示“攻击”。
- 有 `sparring` 时目前显示暂不可用。
- 点击“交谈”会发送 `talk` 并关闭弹框。
- `DialogueMsg` 和 `StoryMsg` 只进入消息区历史，不在弹框内再嵌套内容卡。

已完成剧情后不应继续攻击的 NPC，需要在后端通过 `hide_npc` 或 NPC actions 设计来控制。

## 背包

`InventoryMsg` 返回：

- money
- item summaries

item summary 当前包含：

- id
- name
- amount
- usable

只有 `usable = true` 的物品显示“使用”按钮。按钮发送：

```json
{"use":"cold_rain_manual"}
```

server 返回的 `UseItemMsg` 第二个字段是已由脚本定义的显示文案，client 直接显示，不做翻译。

## 战斗 UI

server 通过 `BattleStateMsg` 提供：

- 玩家/敌人气血、内力、行动。
- 玩家/敌人 active effects。
- 主动招式冷却。
- 当前准备武功的主动招式列表。

client 负责：

- 计算百分比。
- 平滑展示玩家和敌人的 AP 变化。
- 禁用行动/内力/cooldown/状态不满足的主动招式卡。
- 点击主动招式卡发送 `perform`。
- 显示 `ActiveSkillFailureMsg`。

战斗消息播放策略：

- `AttackMsg` 只表示进入战斗，不播放攻击动画。
- `CombatNormalMsg` 和 `ActiveSkillMsg` 会进入前端战斗事件队列。
- 队列事件开始播放时才写入消息历史和战斗日志，并播放对应剪影动作。
- `CombatSettlementMsg` 排在队列末尾，最后一击播完后再关闭战斗面板。
- 如果 server 先发了非战斗 `PlayerStatsMsg`，client 会等队列清空后再退出战斗界面。

## 武学面板

server 通过 `ArtsMsg` 提供已学武功列表。client 在非战斗状态下用武学面板显示：

- 武功名和类型。
- 当前等级和上限。
- 学习要求。
- 已解锁普通招式和主动招式。
- 下一阶段解锁项。

可训练的非基础武功显示“修炼/Train”按钮，点击发送：

```json
{"train":"cold_rain_secret"}
```
