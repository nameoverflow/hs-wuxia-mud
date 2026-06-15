# 角色属性与派生数值设计

本文定义玩家角色属性、资源、派生战斗数值和后续落地顺序。它是 [角色养成系统设计](./character-progression.md) 的数值底座：养成文档回答“玩家怎么成长”，本文回答“成长后的属性如何进入战斗、学习、行动和 UI”。

## 设计目标

- 保持无全局角色等级。角色强弱由先天资质、武功、内力上限、装备、心法、状态和实战经验共同表达。
- 把长期事实、运行态和派生数值分开，避免把装备、武功、buff、心法加成散落到战斗代码里。
- 数值公式第一版要足够简单，便于测试和调参；所有常数集中放在派生属性模块，不写死在战斗流程中。
- 先覆盖单挑 PvE。多人围攻、交易、复杂门派心法可以复用这套属性层，但不在第一版强行实现。

## 状态分层

角色相关状态按四层处理：

| 层 | 是否持久化 | 示例 | 说明 |
| --- | --- | --- | --- |
| Profile | 是 | 名字、性别、外貌、称号、门派、师父、辈分 | 展示和身份系统，不直接参与战斗公式 |
| Persistent growth | 是 | 先天属性、武功等级、熟练度、潜能、实战经验、内力上限、装备实例、心法实例 | 玩家长期资产和成长事实 |
| Runtime state | 视情况 | 当前 HP/Qi/Jing、busy、战斗 AP、状态效果、房间掉落 | 会随 tick 或操作变化；战斗 AP 不保存 |
| DerivedStats | 否 | attack、defense、dodge、parry、maxHp、maxQi、maxJing、regen、loadLimit | 每次需要时由长期事实和运行态重算 |

第一版实现时，`DerivedStats` 可以按需计算，不必缓存。后续如果性能需要，可以在 `GameState` 里缓存并在装备、武功、状态变更后打 dirty 标记。

## 角色基础资料

### Profile

建议最终模型：

```haskell
data CharacterProfile = CharacterProfile
  { profileName :: Text
  , profileGender :: Gender
  , profileAppearance :: AppearanceScore
  , profileTitle :: Maybe Text
  , profileFaction :: Maybe FactionId
  , profileTeacher :: Maybe CharId
  , profileSeniority :: Maybe Int
  }
```

玩家角色创建时必须选择 `gender`，并选择或随机得到一个 `appearance` 数值。NPC YAML 可以暂时缺省这些字段，读取时使用 `UnknownGender` 和中性容貌值兼容旧内容。

第一版字段：

| 字段 | 类型 | 用途 | 是否影响战斗 |
| --- | --- | --- | --- |
| `gender` | `male | female`，NPC 可为 `unknown` | 称谓、剧情条件、门派/任务条件、头像生成提示 | 否 |
| `appearance` | `0..10` 整数 | 容貌数值，映射文本描述、头像立绘和剧情条件 | 否 |

`gender` 不能提供任何数值优劣。它可以作为剧情和身份条件，例如某些 NPC 的称呼、某些门派或剧情分支，但不能影响攻击、防御、学习效率或资源恢复。

`appearance` 也不能进入战斗、学习效率或资源恢复公式。它只表示美丑与气质呈现，用于：

- 玩家面板的容貌文本。
- 头像立绘选择或生成提示。
- 剧情条件、NPC 态度和少量社交/身份分支。

第一版取值范围为 `0..10`，玩家创建时可以自定义该数值，默认值为 `5`。

```yaml
profile:
  gender: female
  appearance: 6
```

容貌值到文本和头像的默认映射：

| 值 | 文本描述 | 头像选择 |
| --- | --- | --- |
| 0 | 面貌残破，令人不敢细看 | `beauty-score-00` |
| 1 | 形貌粗陋，眉眼多有乖戾 | `beauty-score-01` |
| 2 | 容色黯淡，不易令人记住 | `beauty-score-02` |
| 3 | 相貌平常，胜在人还干净 | `beauty-score-03` |
| 4 | 五官端正，已有几分顺眼 | `beauty-score-04` |
| 5 | 清清爽爽，不惹眼也不失礼 | `beauty-score-05` |
| 6 | 眉目清秀，举止间有些风致 | `beauty-score-06` |
| 7 | 容貌出众，行在人群里很难被忽略 | `beauty-score-07` |
| 8 | 明艳或俊逸，足以让旁人多看一眼 | `beauty-score-08` |
| 9 | 姿容极盛，近乎江湖传闻中的人物 | `beauty-score-09` |
| 10 | 风华照人，几可称倾城之色 | `beauty-score-10` |

头像立绘优先使用 `gender + appearance + 当前门派/装备` 生成或选择。如果某些分数暂无专属头像资产，客户端可以临时使用最近的可用分数，但协议和存档仍保存精确数值。

剧情条件示例：

```yaml
conditions:
  - type: player_appearance_at_least
    value: 7
  - type: player_gender
    value: female
```

### 先天属性

第一版只公开三个战斗/行动先天属性：

| 字段 | 中文名 | 初始范围 | 普通上限 | 主要用途 |
| --- | --- | --- | --- | --- |
| `strength` | 臂力 | 12-24 | 30 | 伤害、招架、负重、重武器门槛 |
| `agility` | 身法 | 12-24 | 30 | AP 速度、命中、闪避、移动/逃跑类 busy 修正 |
| `vitality` | 根骨 | 12-24 | 30 | HP、Qi、Jing 上限和恢复、受伤减免 |

不设置“悟性”或等价学习天赋。长线 MUD 中学习效率属性很容易变成最优解，玩家会在创建角色时无脑堆高，最终既破坏长期成长，也让属性选择失去区分度。学习速度和上限应由老师、秘籍、场地、门派、实战经验、busy 时间和资源消耗控制，而不是由一个永久先天属性决定。

创建角色建议总点数为 `54`，单项最小 `12`、最大 `24`，默认分配为：

```yaml
innate:
  strength: 18
  agility: 18
  vitality: 18
```

NPC 内容可以使用更宽范围：

| 类型 | 建议范围 |
| --- | --- |
| 普通村民 | 6-12 |
| 新手 NPC | 10-18 |
| 普通江湖人 | 16-24 |
| 精英敌人 | 24-32 |
| 掌门、剧情 Boss | 32-45 |

`luck` 暂不进入第一版属性。容貌已经由 `appearance` 数值表达；福缘类效果先通过剧情 flag、任务条件或随机事件 seed 处理，不作为创建角色可堆数值。

### 后天属性修正

先天属性不应频繁变化。后天成长通过修正项进入 `effectiveAttr`：

```text
effectiveStrength      = innateStrength + trainedStrength + equipmentStrength + techniqueStrength + statusStrength
effectiveAgility       = innateAgility + trainedAgility + equipmentAgility + techniqueAgility + statusAgility
effectiveVitality      = innateVitality + trainedVitality + equipmentVitality + techniqueVitality + statusVitality
```

第一版 `trained*` 推荐使用很慢的基础功折算：

| 修正 | 来源 | 公式 |
| --- | --- | --- |
| `trainedStrength` | 基础拳法 | `basic_fist_level / 20` |
| `trainedAgility` | 基础轻功 | `basic_lightness_level / 20` |
| `trainedVitality` | 基础内功 | `basic_internal_level / 20` |

所有除法均向下取整。`effectiveAttr` 第一版建议软上限 `60`，超过后仍可显示，但公式中按 `60` 参与计算。

## 资源定义

| 资源 | 是否持久化 | 作用 | 恢复方式 |
| --- | --- | --- | --- |
| HP / 气血 | 保存当前值 | 生存资源，归零则战败、死亡或昏迷 | tick 恢复、治疗、休息、药物 |
| Qi / 真气 | 保存当前值和长期上限 | 主动招式、打坐、内功效果 | tick 恢复、打坐、药物、心法 |
| Jing / 精神 | 保存当前值 | 学习、读书、自研、持续动作的行动成本 | tick 恢复、睡眠、休息、药物 |
| AP / 行动点 | 不保存，仅战斗态 | 决定战斗出手节奏和主动招式门槛 | 战斗 tick 增长 |
| Potential / 潜能 | 保存 | 师父学习、自研的消耗货币 | 战斗、任务、job 奖励 |
| CombatExp / 实战经验 | 保存 | 限制武功等级上限，也参与称号或阅历 | 战斗、任务、job 奖励 |
| Money / 金钱 | 保存 | 购买、修理、交易、门派消耗 | 任务、掉落、交易 |

第一版新增 `Jing` 后，`Potential` 只表示“可学习的积累”，`Jing` 表示“一次行动的精力”。这样 `learn/study/research` 不再只是扣潜能的即时按钮。

## DerivedStats

### 建议数据结构

```haskell
data DerivedStats = DerivedStats
  { dsMaxHp :: Int
  , dsMaxQi :: Int
  , dsMaxJing :: Int
  , dsQiRegen :: Double
  , dsJingRegen :: Double
  , dsAttack :: Int
  , dsDefense :: Int
  , dsHit :: Int
  , dsDodge :: Int
  , dsParry :: Int
  , dsDamageBonus :: Int
  , dsDamageReduction :: Int
  , dsLoadLimit :: Int
  }
```

计算入口建议：

```haskell
deriveStats :: World -> Character -> EquipmentState -> TechniqueState -> ActiveEffects -> DerivedStats
```

如果第一版还没有装备实例和心法实例，可以先传空结构。战斗代码只能消费 `DerivedStats`，不能直接读取装备、心法或 buff 细节。

### 基础变量

本文公式使用以下中间量：

```text
str = clamp 1 60 effectiveStrength
agi = clamp 1 60 effectiveAgility
vit = clamp 1 60 effectiveVitality

basicInternal = level(basic_internal)
basicLightness = level(basic_lightness)
basicFist = level(basic_fist)
basicSword = level(basic_sword)

enabledInternal = enabled internal art level, missing = 0
enabledLightness = enabled lightness art level, missing = 0

internalPower = floor(basicInternal * 0.5) + enabledInternal
lightnessPower = floor(basicLightness * 0.5) + enabledLightness
```

### 上限和恢复

第一版公式：

```text
combatExpHpBonus = min 80 (combatExp / 2000)
combatExpJingBonus = min 60 (combatExp / 3000)

maxHp = 80
      + vit * 5
      + internalPower * 4
      + combatExpHpBonus
      + equipment.hp
      + technique.hp
      + status.maxHp

maxQi = storedMaxQi
      + vit * 2
      + internalPower * 6
      + equipment.qi
      + technique.qi
      + status.maxQi

maxJing = 80
        + vit * 4
        + internalPower * 2
        + combatExpJingBonus
        + equipment.jing
        + technique.jing
        + status.maxJing

qiRegen = 1.0
        + vit / 25.0
        + internalPower / 30.0
        + equipment.qiRegen
        + technique.qiRegen
        + status.qiRegen

jingRegen = 0.5
          + vit / 40.0
          + internalPower / 60.0
          + equipment.jingRegen
          + technique.jingRegen
          + status.jingRegen
```

说明：

- `storedMaxQi` 是玩家通过打坐、剧情或物品永久提升的真气上限。它继续持久化，避免打坐收益被派生公式覆盖。
- `maxHp/maxJing` 可以完全派生，不必额外保存长期上限。
- 当前 HP/Qi/Jing 在派生上限降低后需要夹紧：`current = min current derivedMax`。

### 负重

```text
loadLimit = 20000
          + str * 1500
          + basicFist * 100
          + equipment.loadLimit
          + technique.loadLimit
          + status.loadLimit
```

单位沿用物品系统内部 weight 单位，不在 UI 暴露具体克数。UI 只显示“轻松、略沉、吃力、寸步难行”等分段。

分段建议：

| 负重比例 | 状态 | 效果 |
| --- | --- | --- |
| `<= 50%` | 轻松 | 无惩罚 |
| `50%-80%` | 略沉 | `agi - 2` 参与派生 |
| `80%-100%` | 吃力 | `agi - 5`，移动 busy 增加 |
| `> 100%` | 过载 | 禁止移动和战斗发起 |

## 战斗数值

### AP 增长

沿用当前 AP 机制，改为基于派生身法：

```text
maxAp = 100
baselineAgility = 18
targetActionSeconds = 2.0

apGainPerSecond = maxAp * agi / (baselineAgility * targetActionSeconds)
```

`agi = 18` 时约 2 秒一次普通行动；`agi = 24` 时约 1.5 秒；`agi = 12` 时约 3 秒。

主动招式仍使用 YAML 里的 `ap_req`。高身法并不降低招式 AP 门槛，只是更快攒到门槛。

### 普通攻击选招

普通攻击继续从当前 `prepared` 的近战武功中选择已解锁 `attack_moves`：

```text
eligible moves =
  prepared Sword/Fist art
  + art level >= move.unlock_level
```

武器系统上线后，选择规则扩展为：

| 武器状态 | 可用类型 |
| --- | --- |
| 无武器 | Fist |
| 剑 | Sword |
| 刀 | Sword 第一版可临时复用，后续新增 Blade |
| 爪/拳套 | Fist |

### 命中、闪避、招架

所有对抗判定使用同一个公式：

```text
contestChance(a, b, minChance, maxChance) =
  clamp minChance maxChance (a / (a + b))
```

命中先对闪避，命中后再对招架。

```text
meleeLevel = selected prepared art level
foundationMelee = selected art foundation level

attackScore = 30
            + move.damage * 4
            + meleeLevel * 7
            + foundationMelee * 2
            + str * 3
            + agi * 2
            + equipment.weaponAttack * 4
            + dsHit
            + status.hit

dodgeScore = 20
           + agi * 5
           + vit * 2
           + lightnessPower * 6
           + equipment.dodge * 4
           + status.dodge

parryArtLevel = max(prepared Sword level, prepared Fist level)

parryScore = 15
           + vit * 4
           + str * 2
           + parryArtLevel * 6
           + equipment.parry * 4
           + status.parry
```

概率：

```text
hitChance = contestChance attackScore dodgeScore 0.05 0.95
parryChance = contestChance parryScore attackScore 0.05 0.85
```

流程：

```text
roll hitChance
  miss -> CombatDodge
  hit -> roll parryChance
    parried -> CombatParry
    not parried -> damage
```

### 伤害

第一版伤害公式：

```text
rawDamage = move.damage
          + meleeLevel / 3
          + foundationMelee / 8
          + max 0 ((str - 10) / 3)
          + equipment.weaponDamage
          + dsDamageBonus
          + status.damageBonus

mitigation = max 0 ((vit - 10) / 4)
           + dsDefense / 6
           + internalPower / 12
           + equipment.armorDefense
           + dsDamageReduction
           + status.damageReduction

finalDamage = clamp 1 999 (rawDamage - mitigation)
```

装备字段拆分：

- `weaponAttack` 主要影响命中压力。
- `weaponDamage` 直接增加伤害。
- `armorDefense` 直接减伤。
- `dodge/parry` 进入对应对抗分数。

这样可以让“锋利但难用的武器”和“稳但不重伤的武器”分开表达。

### 主动招式

主动招式仍由 YAML 定义 `damage/heal/cost/ap_req/cd/req_status/req_arts`。第一版接入派生层后：

```text
activeDamage = skill.damage
             + requiredOrPreparedArtLevel / 2
             + dsDamageBonus
             + skillSpecificBonus
             - activeMitigation

activeMitigation = dsDefense / 8
                 + internalPower / 16
                 + status.damageReduction

activeHeal = skill.heal
           + internalPower / 3
           + technique.healBonus
           + status.healBonus
```

`requiredOrPreparedArtLevel` 的选择顺序：

1. 如果 `req_arts` 非空，取其中最高等级。
2. 否则取当前 `prepared/enabled` 中与该招式所在武功对应的等级。
3. 都没有则为 0，通常不会发生，因为招式已经来自已准备武功。

主动招式暂不走闪避/招架判定，除非 YAML 明确新增字段：

```yaml
defense: dodge | parry | none
```

该字段不在第一版必须实现。

## 学习和养成成本

现有规则保留：

```text
artProgressRequired(targetLevel) = targetLevel * targetLevel * 10
combatExpRequired(targetLevel) = targetLevel * targetLevel * 10
```

新增 `Jing` 后，动作成本建议如下。

### Learn

向师父学习，主消耗为潜能和精神：

```text
potentialCost = times
jingCostPerLesson = max 5 (12 + currentLevel / 2)
progressPerLesson = 6 + teacherBonus + factionBonus
```

第一版为了兼容当前“每次学习直接给满进度”的节奏，可以先实现：

```text
progressGain = artProgressRequired(targetLevel)
```

同时扣除 `jingCostPerLesson`。后续再把 `progressPerLesson` 切回真实多次学习。

`teacherBonus` 来自老师质量、师承关系或门派建筑，不来自玩家先天属性。这样玩家的长期学习路线由“去哪学、跟谁学、付出多少资源和时间”决定，而不是创建角色时选一个永久最优项。

### Practice

练习偏体力成本。不同武功类型消耗不同资源：

| 类型 | 成本 |
| --- | --- |
| Fist/Sword | HP 或 Jing |
| Lightness | Jing，可能增加移动 busy |
| Internal | Qi 或 Jing |

公式：

```text
practiceBusySeconds = clamp 1.0 5.0 (3.0 - agi / 40.0)
practiceJingCost = max 5 (10 + currentLevel / 2)
practiceHpCost = max 0 (8 + currentLevel / 2 - vit / 4)
practiceProgress = 6 + currentLevel / 4 + trainingGroundBonus
```

第一版仍可保持“一次 practice 升一级”，但要先接入 busy 和成本；否则玩家仍然是在点按钮升级。

### Study

读书或研读秘籍，主消耗为精神：

```text
studyJingCost = max 8 (18 + currentLevel / 2)
studyProgress = 8 + bookQualityBonus + quietPlaceBonus
```

秘籍 item 后续应支持等级带：

```yaml
use:
  type: learn_art
  art: cold_rain_secret
  level: 1
  min_level: 1
  max_level: 20
  study_quality: 2
```

`max_level` 只限制通过读书推进的上限，不限制师父学习或实战自研。

### Research

自研消耗潜能和精神，受实战经验门槛限制：

```text
potentialCost = 1
jingCost = max 10 (20 + currentLevel)
researchProgress = 4 + combatInsightBonus + secludedPlaceBonus
```

`combatInsightBonus` 可以先为 0，后续根据最近战斗、敌人类型或门派特性增加。

### Meditate

打坐继续提升长期 `storedMaxQi`，但加入 busy 和内功影响：

```text
qiCost = amount
busySeconds = clamp 2.0 10.0 (amount / 20.0)
maxQiGain = max 1 (amount / 20 + internalPower / 50)
jingCost = max 5 (amount / 10)
```

当前 `{"meditate":40}` 的行为可以迁移为：

```text
consume 40 Qi
consume 5 Jing
busy 2 seconds
gain at least 2 maxQi
```

## 战斗奖励

当前战斗奖励按敌人 HP 和基础属性估算。接入 `DerivedStats` 后，改为相对战力奖励：

```text
enemyPower = dsMaxHp / 4
           + dsAttack
           + dsDefense
           + dsDodge / 2
           + dsParry / 2

playerPower = same formula for player

ratio = enemyPower / max 1 playerPower
rewardScale =
  ratio < 0.35 -> 0.1
  ratio < 0.60 -> 0.4
  ratio < 1.50 -> 1.0
  ratio < 2.50 -> 1.2
  otherwise    -> 0.5

combatExpGain = max 1 (enemyPower / 20 * rewardScale)
potentialGain = max 1 (combatExpGain / 2)
```

说明：

- 敌人太弱时奖励衰减，减少刷低级怪。
- 敌人略强时奖励略高，鼓励挑战。
- 敌人远强时奖励降低，防止蹭死高等级敌人获得异常收益。
- 多人围攻上线前，奖励归属仍按当前单挑处理。

## UI 展示

玩家面板建议分三块：

1. 基础：
   - 性别、容貌。
   - 臂力、身法、根骨。
   - 门派、师承、称号。

2. 状态：
   - 气血、真气、精神。
   - 负重状态。
   - 当前 busy 或状态效果。

3. 战斗：
   - 攻击、防御、命中、闪避、招架。
   - 当前准备武功、启用武功、武器。

不要把所有中间公式都展示给玩家。详细数值可以在 dev/debug 面板展示，正式 UI 用“攻击：初窥门径、身法：轻灵”等中文分段。

分段建议：

| 数值 | 文案 |
| --- | --- |
| `< 30` | 平平 |
| `30-59` | 稳健 |
| `60-99` | 出众 |
| `100-159` | 老练 |
| `160+` | 惊人 |

## YAML 和存档迁移

### YAML 第一版

旧格式：

```yaml
attr:
  hp: 114
  qi: 100
  max_qi: 100
  qi_regen: 5.0
  str: 514
  agi: 19
  vit: 18
```

新格式：

```yaml
attr:
  hp: 114
  qi: 100
  jing: 120
  max_qi: 100
  innate:
    strength: 18
    agility: 18
    vitality: 18
profile:
  gender: female
  appearance: 6
```

兼容读取规则：

- 如果存在 `innate`，使用新字段。
- 如果没有 `innate`，从旧字段迁移：
  - `strength = normalizeOldStrength(str)`
  - `agility = clamp 6 45 agi`
  - `vitality = clamp 6 45 vit`
- `jing` 缺失时默认 `maxJing`，或使用 `120`。
- `profile.gender` 缺失时，玩家创建流程必须补选；旧 NPC 或测试模板可读为 `unknown`。
- `profile.appearance` 缺失时默认为 `5`；读取时夹紧到 `0..10`。
- `qi_regen` 旧字段先保留读取，但新派生层上线后不再作为事实源。

旧 `str=514` 是早期平衡占位，不能直接作为臂力。迁移函数：

```text
normalizeOldStrength old =
  if old > 100 then 18 + min 12 ((old - 100) / 50)
  else clamp 6 45 old
```

这会把 `514` 映射到 `26`，保留“木人/测试角色偏强”的感觉，但不会打爆新公式。

### PlayerSave

新增字段建议：

```json
{
  "version": 3,
  "profile": {
    "gender": "female",
    "appearance": 6
  },
  "character": {
    "innate": {
      "strength": 18,
      "agility": 18,
      "vitality": 18
    },
    "hp": 114,
    "qi": 100,
    "jing": 120,
    "max_qi": 100
  }
}
```

迁移规则：

1. `version <= 2` 的存档补 `innate`。
2. 当前 `charStrength/charAgility/charVitality` 继续读入，但写回时使用新字段。
3. 缺失 `jing` 时按新派生 `maxJing` 回满。
4. 缺失 `profile.gender` 时，旧测试存档可临时设为 `unknown`；正式创建角色必须选择 `male` 或 `female`。
5. 缺失 `profile.appearance` 时设为 `5`；如果旧存档里是文本，迁移为 `5` 并丢弃文本，后续头像和描述统一由数值生成。
6. 迁移后保存为 `version = 3`。

## 实现计划

当前实现状态：

- Phase A 已落地：类型、存档迁移、YAML 兼容、角色面板和 `deriveStats` 测试已完成。
- Phase B 已落地：AP 增长、普通攻击、主动招式伤害/治疗、状态 modifier、战斗快照资源上限均通过 `DerivedStats`。
- Phase C 已部分落地：`Jing` 已用于 learn/practice/study/research/meditate 消耗，并随 tick 恢复；busy runtime、休息/睡眠恢复动作仍待实现。
- Phase D/E 仍待实现：当前只有装备/心法 modifier 的派生入口，还没有装备实例、心法实例和对应 UI。

### Phase A：文档和类型

- 增加 `InnateAttrs`、`DerivedStats`、`CharacterVitals` 类型。
- `Character` 增加 `charInnate`、`charJing`，玩家存档增加 `profile.gender` 和 `profile.appearance`。
- YAML 和存档解析保持旧字段兼容。
- 增加 `deriveStats` 纯函数和单元测试。

完成标准：

- 旧 YAML 和旧 save 能启动。
- 默认玩家属性面板能显示性别、容貌、臂力、身法、根骨、精神。
- `deriveStats` 对同一个输入输出稳定。

### Phase B：战斗接入

- `Game.Combat` 的 `attackPower/dodgePower/parryPower/computeDamage` 改为消费 `DerivedStats`。
- AP 增长改用派生身法。
- `BattleState` 保留当前 HP/Qi，派生上限用于战斗快照和 HoT/治疗上限。
- 主动招式伤害和治疗接入派生公式。

完成标准：

- 现有战斗测试通过。
- 沉默木人和纸伞客的 TTK 不出现数量级变化。
- 命中、闪避、招架在日志中仍能稳定出现。

### Phase C：精神和 busy

- 新增 Jing tick 恢复。
- `learn/practice/study/research/meditate` 扣 Jing。
- busy runtime state 接入这些动作。
- UI 已展示精神；busy 展示待实现。

完成标准：

- 玩家精神不足时不能继续学习/研读/自研。
- 睡眠或休息可以恢复精神。
- 养成动作不再是零时间连续点击。

### Phase D：装备实例

- 新增装备槽、装备实例、穿脱命令。
- 装备属性进入 `DerivedStats`。
- 战斗消耗耐久，耐久为 0 时属性失效。
- 修理和负重规则上线。

完成标准：

- 同一武功下，换武器能改变命中/伤害。
- 穿护甲能降低伤害但可能影响身法或负重。
- 装备实例保存和读取稳定。

### Phase E：心法

- 新增 Technique/Xinfa template 和 instance。
- 支持装备、修炼、战斗中成长。
- 心法 modifier 进入 `DerivedStats`。
- 攻击心法招式复用主动招式管线。

完成标准：

- 心法不需要修改战斗主流程，只通过派生层和主动招式进入系统。
- 同类心法装备限制清晰。

## 测试规划

### 纯函数测试

- `normalizeOldStrength 514 == 26`。
- 默认 `18/18/18` 的 `maxHp/maxQi/maxJing` 在预期区间。
- 提升基础内功会提高 `maxHp/maxQi/qiRegen`。
- 提升基础轻功会提高 `dodgeScore` 和 AP 增长。
- 装备防具只影响防御/减伤，不影响学习成本。

### Gameplay 测试

- 旧默认玩家能登录、移动、战斗、学习、使用秘籍。
- 战斗胜利奖励仍发 `combat_exp` 和 `potential`。
- 精神不足时 `learn/study/research` 返回结构化失败。
- `meditate` 同时扣 Qi/Jing，增加 `maxQi`。
- 旧 save 自动迁移后能再次保存。

### 数值回归

每次改公式时记录四组样例：

| 样例 | 目标 |
| --- | --- |
| 默认玩家 vs 沉默木人 | 新手训练战，应稳定可胜 |
| 默认玩家 vs 纸伞客 | 剧情战，应有压力但可通过 |
| 高身法低臂力角色 vs 同级敌人 | 闪避明显，但伤害偏低 |
| 高臂力低身法角色 vs 同级敌人 | 伤害高，但被闪避和挨打更多 |

这些样例可以先写成 server-side deterministic combat test，后续再接命令 API playtest。

## 近期取舍

- 先做派生属性，不先做心法。心法依赖派生层，否则会把加成塞进战斗代码。
- 先做精神和 busy，不继续新增养成动词。现有动词已经够用，缺的是成本和时间。
- 先兼容旧 YAML，不一次性改完全部内容脚本。迁移函数能降低内容改动风险。
- 先保持单挑奖励，等房间对象和奖励归属稳定后，再处理多人围攻。
- 先用简单公式和测试固定手感，再引入更复杂的门派、武器、心法 hook。
