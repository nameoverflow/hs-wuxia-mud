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
{"choose":"cold_rain_accept_wine"}
{"use":"cold_rain_manual"}
{"say":"..."}
{"other":"view"}
{"other":"quests"}
{"other":"inventory"}
```

## Server 响应

响应是 `ActionResp` 的 Aeson generic JSON。主要消息：

- `MoveMsg`
- `ViewMsg`
- `AttackMsg`
- `CombatNormalMsg`
- `CombatSettlementMsg`
- `SkillMsg`
- `SkillFailureMsg`
- `BattleStateMsg`
- `StoryMsg`
- `QuestLogMsg`
- `InventoryMsg`
- `RewardMsg`
- `UseItemMsg`
- `DialogueMsg`
- `SayMsg`
- `PlayerStatsMsg`

`PlayerResp` 是 `(PlayerId, ActionResp)`，server 只把响应发给对应玩家。

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

- 玩家剧情状态：quest stages、flags、hidden NPCs、pending choices。
- 背包。
- 金钱。
- 已学武功。
- 已准备武功。

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

如果后续要做长期角色成长，应先决定哪些运行态进入 `PlayerSave`，哪些进入真正数据库表。
