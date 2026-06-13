# 角色养成系统设计

本文定义当前角色养成模型。核心原则：角色本身没有等级，长期成长主要落在武功等级、武功熟练度、潜能、实战经验和内力上限上。

## 目标

- 角色无全局等级。
- 所有可成长能力都表示为武功或基础功等级。
- 角色初始自带基础功：
  - 基础内功
  - 基础轻功
  - 基础剑法
  - 基础拳法
- 除基础功外，还存在具体武功：
  - 内功
  - 轻功
  - 剑法
  - 拳法
- 学习具体武功需要对应基础功达到门槛。
- 基础功不能直接修炼升级，而是由具体武功升级自动带动。
- 武功可定义招式；招式按武功等级解锁。
- 潜能用于向师父学习和自研。
- 实战经验用于限制可提升的武功等级。
- 打坐会消耗当前 Qi 并提升 `charMaxQi`。

## 分类模型

`MartialArt` 使用 `type` 表示大类：

```yaml
type: foundation | internal | lightness | sword | fist
```

当前不保留旧分类别名；新武功必须使用 `foundation/internal/lightness/sword/fist` 之一。

每门武功都可以带以下字段：

```yaml
id: cold_rain_secret
name: "听雨残谱"
type: sword
desc: "..."
foundation: basic_sword
requires:
  - art: basic_sword
    level: 1
max_level: 20
attack_moves: []
active_skills: []
```

- `foundation` 可选，指向一门 `type: foundation` 的基础功。
- `requires` 可选，列出学习门槛。
- `max_level` 可选，默认 100。
- `attack_moves` 是普通攻击招式。
- `active_skills` 是玩家可主动 `perform` 的招式。

玩家已学武功记录为 `ArtEntity`：

```yaml
id: cold_rain_secret
level: 3
progress: 0
```

- `level` 是当前等级。
- `progress` 是通往下一等级的熟练度。
- 下一等级所需熟练度当前为 `targetLevel * targetLevel * 10`。

## 基础功

基础功也是一种 `MartialArt`，但 `type: foundation`。

示例：

```yaml
id: basic_sword
name: "基础剑法"
type: foundation
desc: "一切剑法的起手。"
max_level: 100
```

基础功的特点：

- 默认由角色模板授予。
- 不配置主动招式。
- 不允许玩家直接 `train basic_sword`。
- 作为学习具体武功的门槛。
- 由具体武功升级自动提升。
- 不进入 `prepared`，战斗不直接消费基础功。

## 具体武功

具体武功声明自己依附的基础功：

```yaml
id: cold_rain_secret
name: "听雨残谱"
type: sword
foundation: basic_sword
requires:
  - art: basic_sword
    level: 3
max_level: 20
```

学习时检查：

- 玩家是否已经拥有 `requires` 中列出的武功。
- 对应武功等级是否达到门槛。
- `level` 必须为正。
- 学习等级不能超过目标武功 `max_level`。

学习成功后：

- 新武功加入角色 `charArt`。
- 初始等级为奖励/秘籍声明的等级，通常为 1。
- 非基础功会自动设为对应类型的 `prepared` 和 `enabled` art。

## 基础功升级规则

基础功等级由同门具体武功升级带动。

当前采用简单、确定的规则：

```text
基础功等级 = max(当前基础功等级, 同门具体武功的新等级)
```

例如：

- 玩家有 `basic_sword` level 1。
- 学会 `cold_rain_secret` level 1。
- 执行 `train cold_rain_secret` 到 level 2。
- `basic_sword` 自动提升到 level 2。

这个规则的优点：

- 不需要额外经验条或角色等级。
- 容易解释：把简单剑法练深，剑法根基自然变厚。
- 学习更难武功的门槛可以直接写成基础功等级。

后续如果需要更慢节奏，可以把规则替换成“基础功经验/熟练度累计”，但外部学习门槛仍然不变。

## 武功升级与养成动作

`train` 是旧客户端兼容入口，服务端等价处理为 `practice`：

```json
{"train":"cold_rain_secret"}
```

```json
{"practice":"cold_rain_secret"}
```

规则：

- 必须已经学会该武功。
- 基础功不能直接训练。
- 达到 `max_level` 后不能继续提升。
- 实战经验必须达到目标等级要求。
- 当前第一版中每次 `practice/train` 会给足够熟练度，使武功提升一级。
- 若该武功有 `foundation`，同步提升对应基础功。
- 升级后同步刷新对应 `prepared/enabled` 中的等级快照。

训练响应：

- 训练成功后发送 `RewardMsg`，`kind = martial_art`，`amount` 为新等级。
- 如果同步提升了基础功，也在同一批奖励里返回基础功的新等级。
- 训练失败用普通错误返回，例如未学会、基础功不能训练、已到上限。

额外养成动作：

```json
{"learn":{"teacher":"cold_rain_innkeeper","art":"cold_rain_secret","times":2}}
{"study":"cold_rain_manual"}
{"research":"cold_rain_secret"}
{"meditate":40}
{"enable":{"type":"sword","art":"cold_rain_secret"}}
{"prepare":{"type":"sword","art":"cold_rain_secret"}}
```

- `learn` 需要 NPC 在同房间且 `Character.teaches` 声明可教该武功，消耗潜能。
- `study` 读取物品 `use.learn_art` 配置，未学会时学习，已学会时推进熟练度。
- `research` 对已学武功自研，消耗 1 点潜能并推进熟练度。
- `meditate` 消耗指定数量当前 Qi，按 `max 1 (amount / 20)` 提升 `charMaxQi`。
- `enable` 用于把已学非基础功启用到对应类型，主动招式会从 `prepared + enabled` 合并暴露。
- `prepare` 用于把已学非基础功准备到对应类型，普通攻击流水线仍主要消费 `prepared`。

战斗胜利会按敌人 HP 和基础属性给少量 `combat_exp` 与 `potential` 奖励。

## 招式解锁

普通招式 `attack_moves` 和主动招式 `active_skills` 都可以声明 `unlock_level`：

```yaml
attack_moves:
  - id: cold_rain_cut
    name: "雨后一刀"
    unlock_level: 1
    damage: 14

active_skills:
  - id: umbrella_spine_eight
    name: "伞骨八刺"
    unlock_level: 5
    cost: 80
```

规则：

- 未声明 `unlock_level` 时默认 1。
- 普通攻击只会从已解锁的 `attack_moves` 中随机选择。
- 客户端战斗主动招式列表只展示已解锁的主动招式。
- 手动 `perform` 未解锁招式会失败。
- 主动招式可选声明 `req_arts`，要求玩家已学指定武功。

## 秘籍学习

秘籍仍是物品。

```yaml
use:
  type: learn_art
  art: cold_rain_secret
  level: 1
  consume: false
```

使用秘籍时：

1. 检查秘籍目标武功是否存在。
2. 检查目标武功的 `requires`。
3. 未满足门槛则返回明确错误。
4. 满足后学习武功。
5. 重复使用不会重复学习，显示 `repeat_message`。

`study` 与 `use` 的差异：

- `use` 保持原语义：用于物品效果，重复使用只显示重复文案。
- `study` 把同一份秘籍作为研读材料，已学会后会继续推进对应武功熟练度。

## 玩家查询

需要提供玩家已学武功列表：

```json
{"other":"arts"}
```

响应应包含：

- 武功 id。
- 名称。
- 类型。
- 当前等级。
- 最大等级。
- 是否基础功。
- 基础功依赖。
- 学习要求。
- 已解锁招式。
- 下一等级会解锁什么。

客户端当前在非战斗状态下用武学面板展示已学武功，并提供可训练武功的按钮。

查询响应使用 `ArtsMsg`，每项为：

```json
{
  "artSummaryId": "cold_rain_secret",
  "artSummaryName": "听雨残谱",
  "artSummaryType": "sword",
  "artSummaryLevel": 3,
  "artSummaryProgress": 0,
  "artSummaryNextProgress": 160,
  "artSummaryMaxLevel": 20,
  "artSummaryFoundation": "basic_sword",
  "artSummaryRequirements": [
    {"artRequirementSummaryId":"basic_sword","artRequirementSummaryName":"基础剑法","artRequirementSummaryLevel":1}
  ],
  "artSummaryUnlockedAttackMoves": ["雨后一刀"],
  "artSummaryUnlockedActiveSkills": ["灯下一刀"],
  "artSummaryNextUnlocks": ["伞骨八刺"]
}
```

客户端不翻译武功名、招式名和描述，只翻译固定字段标题。

## 世界校验

server 启动时应拒绝不一致的武功配置：

- `max_level` 必须为正。
- `foundation` 必须引用存在的基础功。
- `requires.art` 必须存在，`requires.level` 必须为正。
- `attack_moves[].unlock_level` 和 `active_skills[].unlock_level` 必须为正。
- `unlock_level` 不应超过该武功 `max_level`。
- `active_skills[].req_arts` 必须引用存在的武功。
- `Character.teaches` 必须引用存在的武功，且可教上限必须为正、不能超过武功 `max_level`。
- 物品和剧情 `learn_art` 的等级必须为正，且不能超过目标武功 `max_level`。

## 存档影响

当前存档保存：

- `version`
- `story`
- `inventory`
- `money`
- `potential`
- `combat_exp`
- `hp/max_hp`
- `qi/max_qi`
- `arts`
- `prepared`
- `enabled`

旧存档缺少的新字段会以默认值读取；旧存档若仍引用旧分类或旧武功 id，应视为不可兼容，需要迁移或重建。

## 高优后续

- 增加武功切换 UI。
- 增加已学/可学/未满足门槛的武学面板。
- 增加秘籍阅读失败的客户端专门提示样式。
- 增加 busy 机制，让 learn/practice/study/research/meditate 不再是瞬时动作。
