# 协议与存档

## WebSocket 连接

server 监听：

```text
127.0.0.1:9160
```

连接后第一条消息必须是 `Login`：

```json
{
  "tag": "Login",
  "username": "tester",
  "password": ""
}
```

登录成功后，server 会创建或加载玩家，并发送当前 `playerView`。

## 客户端动作

动作统一包在：

```json
{
  "tag": "NetPlayerAction",
  "contents": { "...": "..." }
}
```

当前支持的 `contents`：

```json
{"go":"North"}
{"talk":"cold_rain_innkeeper"}
{"attack":"paper_umbrella_killer"}
{"perform":"power_strike"}
{"train":"cold_rain_secret"}
{"use":"cold_rain_manual"}
{"say":"..."}
{"other":"view"}
{"other":"quests"}
{"other":"inventory"}
{"other":"arts"}
```

`train` 只用于已学非基础武功升级。基础功由具体武功升级带动，不能直接训练。

`other: "arts"` 用于查询已学武功、等级、学习门槛和已解锁招式。

## Server 响应

响应是 `ActionResp` 的 Aeson generic JSON。主要消息：

- `MoveMsg`
- `ViewMsg`
- `AttackMsg`
- `CombatNormalMsg`
- `CombatSettlementMsg`
- `ActiveSkillMsg`
- `ActiveSkillFailureMsg`
- `BattleStateMsg`
- `StoryMsg`
- `QuestLogMsg`
- `InventoryMsg`
- `ArtsMsg`
- `RewardMsg`
- `UseItemMsg`
- `DialogueMsg`
- `SayMsg`
- `PlayerStatsMsg`
- `SystemMsg`
- `ErrorMsg`

`PlayerResp` 是 `(PlayerId, ActionResp)`，server 只把响应发给对应玩家。

server 不直接返回英文 UI 句子。固定系统文案使用结构化消息：

```json
{"tag":"SystemMsg","contents":{"systemMessageKey":"welcome","systemMessageParams":{"users":"tester"}}}
{"tag":"ErrorMsg","contents":{"errorSummaryCode":"unable_to_move","errorSummaryParams":{"direction":"North","room":"冷雨渡口"}}}
```

client 根据 `systemMessageKey` / `errorSummaryCode` 和参数做本地化。剧情文本、NPC 名字、房间描述、武功招式文案仍由脚本内容决定，不放进 UI i18n 表。

战斗消息中的动作描述也已结构化：

```json
{"kind":"script","text":"一剑刺出。"}
{"kind":"effect_tick","effectId":"bleeding","effectName":"血痕","effectKind":"dot","amount":8}
```

`CombatNormalMsg` 和 `ActiveSkillMsg` 使用这个对象，client 负责把持续伤害、恢复等固定句式格式化。`script` 类型的 `text` 是脚本内容，直接显示。

`ActiveSkillFailureMsg` 不再是文本，而是原因对象：

```json
{"reason":"need_ap","required":60,"current":0}
{"reason":"cooldown","remaining":2}
{"reason":"missing_status","statuses":["wind_stance"]}
{"reason":"unavailable","activeSkillId":"power_strike"}
```

`PlayerStatsMsg` 的状态字段是稳定状态码：`normal`、`in_battle`、`dead`、`banned`。

## 存档

存档类型是 `PlayerSave`，当前 JSON 字段：

```json
{
  "player_id": "tester",
  "story": {},
  "inventory": {},
  "money": 0,
  "arts": {},
  "prepared": {}
}
```

保存内容：

- 玩家剧情状态：quest stages、flags、hidden NPCs。
- 背包。
- 金钱。
- 已学武功，包括基础功和具体武功的等级。
- 已准备武功。基础功不需要进入 prepared。

加载流程：

1. server 使用 `default_player.yaml` 创建玩家。
2. 如果 `saves/<player>.json` 存在，覆盖 story/inventory/money/arts/prepared。

保存时机：

- `runAndResponse` 执行出非空响应后调用 `saveAllPlayerSaves`。
- 玩家断线时清除 battle、把状态置为 normal，并保存该玩家。

## 当前未保存内容

当前没有完整数据库系统。以下内容不是持久化目标，或只以世界配置为准：

- 当前房间位置没有写入 `PlayerSave`。
- 当前 HP/Qi 没有写入 `PlayerSave`。
- 进行中的 battle 不保存。
- NPC 全局死亡/复活倒计时不保存。
- 世界内容来自 YAML，每次 server 启动重新加载。

当前角色成长只增加武功等级，已经落在 `arts` 字段里，不需要新增顶层存档字段。如果后续增加潜能、阅历、长期 HP/Qi 成长，应先决定哪些运行态进入 `PlayerSave`，哪些进入真正数据库表。
