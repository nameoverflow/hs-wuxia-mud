# 客户端 UI

客户端位于 [client/](../client)，是原生 HTML/CSS/JavaScript WebSocket 客户端。

## 当前职责

- 登录并连接 `ws://127.0.0.1:9160`。
- 渲染玩家 HP/Qi/AP 和状态。
- 渲染当前房间、出口方位图、NPC 列表。
- 点击方位图节点直接移动。
- 点击 NPC 打开交互弹框。
- 显示剧情消息和剧情选项。
- 显示任务、背包、奖励消息。
- 背包中可使用物品显示“使用”按钮。
- 战斗中显示敌我状态、技能卡、冷却、前置状态需求。
- 支持中/英文 UI 固定字段 i18n。

## UI i18n 边界

只对 UI 固定字段做 i18n，例如：

- 面板标题。
- 按钮。
- 状态名。
- 技能卡上的固定标签。
- 空列表占位。

不翻译：

- 剧情文本。
- NPC 名字。
- 房间名/描述。
- 物品名/描述。
- 武功名/描述。
- 技能名/描述。

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

- 玩家/敌人 HP、Qi、AP。
- 玩家/敌人 active effects。
- 技能冷却。
- 当前准备武功的技能列表。

client 负责：

- 计算百分比。
- 禁用 AP/Qi/cooldown/状态不满足的技能卡。
- 点击技能卡发送 `perform`。
- 显示 `SkillFailureMsg`。
