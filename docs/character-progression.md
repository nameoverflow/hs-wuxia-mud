# 角色养成系统设计

本文定义当前角色养成模型。核心原则：角色本身没有等级，成长全部落在武功等级上。

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
- 非基础功会自动设为对应类型的 prepared art。

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

## 武功升级

玩家通过动作升级已学武功：

```json
{"train":"cold_rain_secret"}
```

规则：

- 必须已经学会该武功。
- 基础功不能直接训练。
- 达到 `max_level` 后不能继续提升。
- 升级后发送武功奖励/提示消息。
- 若该武功有 `foundation`，同步提升对应基础功。

当前不引入角色经验、潜能、银两或时间消耗；后续可以在 `train` 前增加资源检查。

训练响应：

- 训练成功后发送 `RewardMsg`，`kind = martial_art`，`amount` 为新等级。
- 如果同步提升了基础功，也在同一批奖励里返回基础功的新等级。
- 训练失败用普通错误返回，例如未学会、基础功不能训练、已到上限。

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
- 物品和剧情 `learn_art` 的等级必须为正，且不能超过目标武功 `max_level`。

## 存档影响

当前存档已经保存 `charArt` 和 `charPrepare`，等级字段在 `ArtEntity` 内，不需要新增顶层存档字段。旧存档若仍引用旧分类或旧武功 id，应视为不可兼容，需要迁移或重建。

## 高优后续

- 增加训练消耗：潜能、阅历、银两或时间。
- 增加武功切换 UI。
- 增加已学/可学/未满足门槛的武学面板。
- 增加秘籍阅读失败的客户端专门提示样式。
