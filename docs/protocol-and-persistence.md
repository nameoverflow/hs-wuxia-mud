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

本地测试入口使用同一个 `Login` 消息，但把 `password` 设为特殊值：

```json
{
  "tag": "Login",
  "username": "tester",
  "password": "__dev_reset"
}
```

只有 server 以 `MUD_DEV_MODE=1` / `true` / `TRUE` 启动时，才接受这个重置请求。重置会删除该玩家的 JSON 存档，清理内存里的玩家、战斗、剧情状态和房间占位，然后按 `default_player.yaml` 创建同名新角色。正常模式下会返回：

```json
{"tag":"ErrorMsg","contents":{"errorSummaryCode":"dev_mode_required","errorSummaryParams":{}}}
```

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
{"talk":"wounded_escort"}
{"talk":"temple_black_clad"}
{"perform":"steady_cut"}
{"train":"weiyuan_sword"}
{"practice":"weiyuan_sword"}
{"learn":{"teacher":"wounded_escort","art":"weiyuan_sword","times":1}}
{"study":"weiyuan_sword_manual"}
{"research":"weiyuan_sword"}
{"meditate":40}
{"enable":{"type":"sword","art":"weiyuan_sword"}}
{"prepare":{"type":"sword","art":"weiyuan_sword"}}
{"use":"weiyuan_sword_manual"}
{"say":"..."}
{"other":"view"}
{"other":"quests"}
{"other":"inventory"}
{"other":"arts"}
```

`train` 是旧客户端兼容入口，当前等价于 `practice`。基础功由具体武功升级带动，不能直接训练。

`other: "arts"` 用于查询已学武功、等级、学习门槛和已解锁招式。

## Server 响应

响应是 `ActionResp` 的 Aeson generic JSON。主要消息：

- `MoveMsg`
- `ViewMsg`
- `AttackMsg`
- `CombatEventMsg`
- `CombatSettlementMsg`
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
{"tag":"ErrorMsg","contents":{"errorSummaryCode":"unable_to_move","errorSummaryParams":{"direction":"North","room":"开封城外破庙口"}}}
```

client 根据 `systemMessageKey` / `errorSummaryCode` 和参数做本地化。剧情文本、NPC 名字、房间描述、武功招式文案仍由脚本内容决定，不放进 UI i18n 表。

战斗事件使用统一的 `CombatEventMsg`。普通攻击、主动招式、DoT/HoT tick 都走这条消息。

典型普通攻击事件：

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
    "hits": [{ "result": "hit", "damage": 16, "heal": null }],
    "visual": {
      "actionId": "rig.fist.punch_a",
      "tags": ["fist", "strike"],
      "durationMs": 450,
      "params": null
    }
  }
}
```

- `hits`：逐段结果，段数等于该动作 manifest 的 `hits` 数（未声明时为 1）。多段招式每段各自判定闪避/招架，`damage` 是命中段之和；顶层 `result` 只要有一段命中就是 `hit`，否则取第一段的结果。
- `visual.durationMs`：动作剪辑时长（不含接近步法），客户端按它缩放整段表现。服务端的行动锁 = 这个时长 + manifest 里 `approach.durationMs`。
- `visual.params`：武学 YAML 里 `animation.params` 原样透传，见 [battle-animation.md](./battle-animation.md)。

`contents.kind` 当前取值：

- `normal`
- `active_skill`
- `effect_tick`

`contents.result` 当前取值：

- `hit`
- `dodge`
- `parry`
- `effect`

`message` 是战斗文本或效果 tick 描述：

```json
{"kind":"script","text":"一剑刺出。"}
{"kind":"effect_tick","effectId":"bleeding","effectName":"血痕","effectKind":"dot","amount":8}
```

`visual` 是表现提示，不是浏览器实现细节。server 只发送动作池、指定动作和语义 tag；图片路径、CSS class、VFX DOM 和具体时长由 client catalog/resolver 决定。

`CombatSettlementMsg` 仍是独立消息。client 会把结算排在战斗动画队列末尾，最后一个 half-turn 播完后再显示胜负并关闭战斗面板。

`ActiveSkillFailureMsg` 不再是文本，而是原因对象：

```json
{"reason":"need_ap","required":60,"current":0}
{"reason":"cooldown","remaining":2}
{"reason":"missing_status","statuses":["wind_stance"]}
{"reason":"unavailable","activeSkillId":"active_skill_id"}
```

`PlayerStatsMsg` 的状态字段是稳定状态码：`normal`、`in_battle`、`dead`、`banned`。

## 存档

存档类型是 `PlayerSave`，当前 JSON 字段：

```json
{
  "version": 5,
  "player_id": "tester",
  "story": {},
  "position": ["kaifeng_city", [0, 1]],
  "inventory": {},
  "money": 0,
  "potential": 20,
  "combat_exp": 1000,
  "profile": {
    "gender": "unknown",
    "appearance": 5
  },
  "character": {
    "desc": "我出身于武学世家。",
    "innate": {
      "strength": 21,
      "agility": 22,
      "vitality": 20
    },
    "hp": 180,
    "max_hp": 180,
    "qi": 164,
    "max_qi": 124,
    "jing": 160
  },
  "hp": 180,
  "max_hp": 180,
  "qi": 164,
  "max_qi": 124,
  "jing": 160,
  "desc": "我出身于武学世家。",
  "innate": {
    "strength": 21,
    "agility": 22,
    "vitality": 20
  },
  "arts": {},
  "prepared": {},
  "enabled": {}
}
```

当前存档版本为 `6`。开封内容完成了一次性资源 ID 迁移，不保留旧城市 ID 的兼容映射；载入更早版本时保留角色数值和仍存在的物品，但重置剧情状态与位置，避免旧任务、flag 和地图 ID 重新进入运行时。

保存内容：

- 玩家剧情状态：quest stages、flags、hidden NPCs。
- 当前地图和房间位置。
- 背包。
- 金钱。
- 潜能和实战经验。
- 角色描述、性别、容貌、先天属性和 HP/Qi/Jing 等长期资源状态。
- 已学武功，包括基础功和具体武功的等级与熟练度。
- 已准备武功。基础功不需要进入 prepared。
- 已启用武功。主动招式会从 prepared/enabled 合并暴露。

加载流程：

1. server 使用 `default_player.yaml` 创建玩家。
2. 如果 `saves/<player>.json` 存在，覆盖 story/position/inventory/money/potential/combat_exp/desc/profile/innate/HP/Qi/Jing/arts/prepared/enabled。
3. 位置存在且仍指向有效房间时，同步修正新旧房间的玩家占位；旧存档没有位置时保留默认出生点。

dev reset 登录流程：

1. client 发送 `Login.password = "__dev_reset"`。
2. server 检查 `MUD_DEV_MODE`。
3. 删除 `saves/<player>.json`。
4. 如果该玩家正在战斗，先释放被锁定的 NPC encounter。
5. 清理内存中的玩家、battle、story 和所有房间内的该玩家占位。
6. 用 `default_player.yaml` 创建同名玩家，不再加载旧存档。

保存时机：

- `runAndResponse` 执行出非空响应后调用 `saveAllPlayerSaves`。
- 玩家断线时释放被锁定的 NPC encounter、清除 battle、把状态置为 normal，并保存该玩家。

## 当前未保存内容

当前没有完整数据库系统。以下内容不是持久化目标，或只以世界配置为准：

- 进行中的 battle 不保存。
- NPC 全局战斗锁、HP/Qi 运行态、死亡状态和复活倒计时不保存。
- 世界内容来自 YAML，每次 server 启动重新加载。

当前角色成长已保存武功等级/熟练度、潜能、实战经验和 HP/Qi/MaxQi。后续如果增加更复杂的门派贡献、装备耐久或属性点，需要继续沿用 `PlayerSave.version` 做迁移。
