# 战斗、武功与物品

## 战斗模型

战斗是一对一 `Battle`，以玩家 id 作为 battle id 存在 `GameState.battles` 中。

每个战斗方有 `BattleState`：

- `battleSkillCd`: 技能冷却。
- `battleAp`: 行动点。
- `battleQi`: 当前内力。
- `battleChar`: 战斗中的角色快照。
- `battleEffects`: 当前状态效果。

tick 时：

1. 技能冷却减少。
2. DoT/HoT 生效。
3. 过期状态移除。
4. 双方恢复 Qi，封顶到 `charMaxQi`。
5. 双方按 `agility * dt` 增加 AP。
6. AP 到 `100` 的一方自动普通攻击。
7. 任一方 HP 小于等于 0 时结算。

## 普通攻击

普通攻击从角色当前准备的 `Technique` 武功中随机选一个 `moves` 条目：

```yaml
moves:
  - id: cold_rain_cut
    name: "雨后一刀"
    msg: "在雨声一断时出刀"
    damage: 14
```

普通攻击当前只造成固定伤害，没有命中、闪避、护甲或属性缩放。

## 主动技能

主动技能由客户端发送：

```json
{"perform":"power_strike"}
```

server 会在当前准备的 `Technique` 武功里查找该 skill，并检查：

- AP 是否达到 `ap_req`。
- Qi 是否足够 `cost`。
- 是否不在 cooldown。
- 是否拥有 `req_status` 中列出的状态。

成功后：

- 消耗 AP 和 Qi。
- 设置 cooldown。
- 根据 `target` 对自己或目标施加伤害、治疗、状态。
- 发送 `SkillMsg`、伤害消息和 `BattleStateMsg`。

失败后：

- 发送 `SkillFailureMsg`，包含具体原因。
- 同时发送当前 `BattleStateMsg`，让 UI 保持同步。

## 状态效果

状态定义在 `effects/status_effects.yaml`：

- `dot`: tick 伤害。
- `hot`: tick 治疗。
- `buff`: 当前主要用于作为技能前置状态。
- `debuff`: 当前主要用于展示/扩展。

技能通过：

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

玩家角色有：

- `charArt`: 已学武功，按 `ArtType` 分组。
- `charPrepare`: 当前准备的武功，按 `ArtType` 分组。

当前支持的 `ArtType`：

- `Technique`
- `Cultivation`
- `Lightness`

使用秘籍学习武功时：

- 如果未学过该武功，加入 `charArt`。
- 如果已学过，保留更高等级。
- 自动把该武功设为对应类型的 prepared art。
- 发出 `RewardMsg`，kind 为 `martial_art`。

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
