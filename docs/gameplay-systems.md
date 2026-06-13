# 战斗、武功与物品

## 战斗模型

战斗是一对一 `Battle`，以玩家 id 作为 battle id 存在 `GameState.battles` 中。

每个战斗方有 `BattleState`：

- `battleActiveSkillCooldowns`: 主动招式冷却。
- `battleAp`: 行动点。
- `battleQi`: 当前内力。
- `battleChar`: 战斗中的角色快照。
- `battleEffects`: 当前状态效果。

tick 时：

1. 主动招式冷却减少。
2. DoT/HoT 生效。
3. 过期状态移除。
4. 双方恢复 Qi，封顶到 `charMaxQi`。
5. 双方按 `agility * dt` 增加 AP。
6. AP 到 `100` 的一方自动普通攻击。
7. 任一方 HP 小于等于 0 时结算。

## 普通攻击

普通攻击从角色当前准备的近战武功中随机选一个已解锁 `attack_moves` 条目：

```yaml
attack_moves:
  - id: cold_rain_cut
    name: "雨后一刀"
    unlock_level: 1
    msg: "在雨声一断时出刀"
    damage: 14
```

近战武功类型按顺序包括：

- `Sword`
- `Fist`

普通攻击当前只造成固定伤害，没有命中、闪避、护甲或属性缩放。

## 主动招式

主动招式由客户端发送：

```json
{"perform":"lamp_cut"}
```

server 会在当前准备的武功中查找已解锁 `ActiveSkill`，并检查：

- AP 是否达到 `ap_req`。
- Qi 是否足够 `cost`。
- 是否不在 cooldown。
- 是否拥有 `req_status` 中列出的状态。

主动武功类型包括：

- `Internal`
- `Lightness`
- `Sword`
- `Fist`

成功后：

- 消耗 AP 和 Qi。
- 设置 cooldown。
- 根据 `target` 对自己或目标施加伤害、治疗、状态。
- 发送 `ActiveSkillMsg`、伤害消息和 `BattleStateMsg`。

失败后：

- 发送 `ActiveSkillFailureMsg`，包含具体原因。
- 同时发送当前 `BattleStateMsg`，让 UI 保持同步。

server tick 中如果双方都满足行动条件，可能在一次响应批次里产生多条战斗行动消息。client 会把这些消息放入战斗事件队列顺序播放；这只是 UI 表现层，不改变 server 侧同 tick 结算结果。

## 状态效果

状态定义在 `effects/status_effects.yaml`：

- `dot`: tick 伤害。
- `hot`: tick 治疗。
- `buff`: 当前主要用于作为主动招式前置状态。
- `debuff`: 当前主要用于展示/扩展。

主动招式通过：

```yaml
effect:
  self:
    - id: dragon_stance
      duration: 15.0
      value: 1
  target: []
```

给自己或目标加状态。

## 武功学习与准备

更完整的角色养成规则见 [角色养成系统设计](./character-progression.md)。本节只记录当前战斗系统如何消费武功数据。

玩家角色有：

- `charArt`: 已学武功，按 `ArtType` 分组。
- `charPrepare`: 当前准备的武功，按 `ArtType` 分组。

当前支持的 `ArtType`：

- `Foundation`
- `Internal`
- `Lightness`
- `Sword`
- `Fist`

使用秘籍学习武功时：

- 先检查目标武功的 `requires` 是否满足。
- 如果未学过该武功，加入 `charArt`。
- 如果已学过，保留更高等级。
- 非基础功会自动把该武功设为对应类型的 prepared art。
- 发出 `RewardMsg`，kind 为 `martial_art`。

训练已学武功时：

- 客户端发送 `{"train":"art_id"}`。
- 基础功不能直接训练。
- 等级不能超过 `max_level`。
- 若武功声明了 `foundation`，训练后同步提升该基础功等级。

## 物品和秘籍

物品当前是轻量背包实体：

- `id`
- `name`
- `desc`
- optional `use`

背包按 `ItemId -> amount` 存储在 `Player.playerInventory`。

当前唯一已实现的使用效果是：

```yaml
use:
  type: learn_art
  art: cold_rain_secret
  level: 1
  consume: false
```

这用于秘籍：剧情给物品，玩家主动使用后才学习武功。非可使用物品不会在背包 UI 显示使用按钮。

## 奖励和掉落

奖励统一通过 `RewardSummary` 给客户端展示：

- `money`
- `item`
- `martial_art`

当前有两类来源：

- `complete_quest`: 发 quest reward 中的 money/items。
- 剧情 action：`give_item`, `give_money`, `learn_art`。

冷雨客栈当前设计：

- 完成章节获得 80 铜钱。
- 杀死纸伞客后获得 `伞骨银针`。
- 与青衣客结尾对话获得 `听雨残谱`。
- 使用 `听雨残谱` 后获得 `听雨残谱` 武功。
