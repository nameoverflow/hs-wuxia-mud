# MUD 调研综合结论与落地路线

本文把此前两类调研整合成一个可执行的设计判断：

- [公开武侠 MUD 服务端源码调研](./public-mud-source-study.md)：传统 LPC/mudlib 的命令、房间、心跳、战斗、武功、师承、任务和存档模型。
- [《夺宝中华》WAP MUD 调研](./duobao-zhonghua-study.md)：PHP/Workerman + Redis + MySQL 的长线养成、在线同步、延迟写回和地图掉落模型。

这两份文档保留为证据和细节。后续做产品/架构决策时，优先看本文。

## 当前已吸收的结论

项目最近几轮改动已经落地了一批早期调研建议：

| 调研结论 | 当前落地状态 |
| --- | --- |
| 强类型核心比直接复制 LPC 对象世界更适合当前项目 | 已采用 Haskell `GameState` + YAML 内容模型 |
| 集中 tick 比每个对象自带 heart beat 更容易测试 | 已有 `gameTickLoop`，并拆出 `tickBattles` / `tickRespawns` |
| 战斗状态应有单一权威源 | 已锁定 NPC encounter，战斗 tick 同步敌方 HP/Qi 到 `world.chars` |
| 战斗不能只做固定伤害 | 普通攻击已拆出命中、闪避、招架、伤害流水线 |
| 主动招式需要 AP/Qi/cooldown/status gate | 已落地 active skill 系统和结构化失败原因 |
| 角色不需要全局等级，成长落在武功和资源上 | 已有武功等级/熟练度、潜能、实战经验、内力上限 |
| 基础功和具体武功应分层 | 已有 `Foundation` 与具体 `Internal/Lightness/Sword/Fist` |
| enable/prepare 是武侠 MUD 的核心模型 | 服务端已支持 `enable` / `prepare`，主动招式从二者合并暴露 |
| learn/practice/study/research/meditate 应分成不同入口 | 服务端已有第一版动作 |
| 剧情任务需要结构化事件系统 | 已有 YAML quest trigger/condition/action 和任务列表 |
| 存档需要版本字段 | `PlayerSave.version = 2` 已落地 |
| tick 不能因为 UI 快照每秒全员保存 | 已有 `dirtyPlayers` 和 tick `SaveDirtyPlayers` 策略 |
| 客户端不应绑定后端英文文案 | 固定系统文案已改为结构化 code + params，客户端 i18n |

因此，后续路线不应再重复“先补 enable/prepare、潜能、师父学习、基础功”这种旧结论；这些已经有服务端骨架。真正的问题是让这些系统变厚，并把现在散落的资源、战斗、物品和持久化边界收束起来。

## 综合设计判断

### 1. 架构方向：保留强类型中心状态，不复制对象世界

传统 LPC mudlib 的优势是内容对象灵活：房间、NPC、武功、任务都可以直接写逻辑。《夺宝中华》的优势是用 Redis 热状态承载在线世界。但二者都有隐式依赖和运行时散乱的问题。

当前项目应该继续走：

```text
YAML content
  -> typed World
  -> centralized GameState
  -> system-specific tick phases
  -> structured PlayerResp
```

后续只在 YAML 表达不足时增加有限 hook，而不是把房间/NPC/武功变成任意脚本对象。

### 2. 状态边界：拆成四层

后续所有新增功能都应先归类到这四层之一：

```text
Persistent state
  玩家长期资产、成长、剧情、位置、装备实例、心法实例

Runtime state
  battle、respawn、NPC 当前 HP/Qi、房间临时掉落、busy、job runtime

Derived state
  attack/defence/dodge/parry/maxHp/maxQi 等由基础属性、装备、武功、心法、状态推导

Ephemeral messages
  战斗日志、房间广播、系统提示、UI 快照
```

《夺宝中华》的 `RoleRow + RoleAttrs + FlushRoleAttrs` 给了一个很清楚的提示：派生属性必须集中计算。当前项目还缺这层。

### 3. Tick 方向：集中 tick，按职责拆 phase

传统 MUD 的 `heart_beat()` 和《夺宝中华》的 monitor timers 都说明一件事：世界推进不应该只挂在玩家点击动作上。

当前已有：

```text
tickBattles
tickRespawns
```

应继续演进为：

```text
tickBattles
tickConditions
tickBusy
tickRespawns
tickRoomObjects
tickNpcAi
tickWorldJobs
tickPersistence
```

原则：

- 高频 phase 只处理必须每秒推进的对象。
- 慢速 phase 不扫全世界，只扫活跃索引。
- UI 快照和保存策略分离。
- 玩家输入只是意图，复杂持续动作由 tick 完成。

### 4. 命令方向：JSON action 继续保留，但要有 action registry

传统 MUD 的命令系统和《夺宝中华》的 `sid+cmd` 都说明：玩家可做动作应该由当前状态/房间/目标动态生成，而不是客户端硬编码。

当前 JSON action 适合 WebSocket 和强类型协议，不需要退回文本命令。但后续应增加服务端 action registry：

```haskell
data AvailableAction
  = Move Direction
  | Talk CharId
  | Attack CharId
  | Use ItemInstanceId
  | Equip ItemInstanceId
  | Study ItemInstanceId
  | RoomAction RoomActionId
  | JobAction JobActionId
```

客户端展示“能做什么”，但权限和校验仍在服务端。这样可以吸收传统 MUD 的房间局部动作和 WAP MUD 的服务端动作白名单，而不照搬短 cmd URL。

### 5. 战斗方向：保留显式 Battle，补 DerivedStats 和装备/武功 hook

当前显式 `Battle` 比传统心跳散打更可控。后续不需要推翻。

下一步应该补的是战斗输入：

```text
Character base attributes
  + prepared/enabled arts
  + equipment instances
  + technique/xinfa modifiers
  + active status effects
  -> DerivedStats
  -> attack pipeline
```

普通攻击流水线已经有命中、闪避、招架和伤害，后续接入：

- 武器攻击和武器类型。
- 护甲/衣服/鞋子防御与闪避。
- 装备耐久消耗。
- 暴击。
- 武功 `on_hit` / `on_defend` hook。
- 内功或心法反震/护体类效果。

多人围攻不要现在做。等装备、掉落、房间对象和奖励归属明确后，再把 `BattleId = PlayerId` 升级为独立 encounter id。

### 6. 养成方向：已有入口，下一步是资源、时间和门派约束

当前服务端已经有：

- `learn`
- `practice/train`
- `study`
- `research`
- `meditate`
- `enable`
- `prepare`

下一步不是继续加新动词，而是让这些动词有不同成本：

| 动作 | 应消耗/约束 | 主要来源 |
| --- | --- | --- |
| `learn` | 潜能、精神、老师可教上限、门派/师承 | 传统 MUD + 《夺宝中华》 |
| `practice` | HP/Qi、busy 时间、场地限制 | 传统 MUD + 《夺宝中华》练功房 |
| `study` | 书籍等级带、精神、读书门槛 | 传统 MUD + 《夺宝中华》秘籍 |
| `research` | 潜能、精神、combat_exp gate | 传统 MUD |
| `meditate` | 当前 Qi、busy 时间、内功影响 | 传统 MUD |
| `enable/prepare` | 已学武功、武器类型、基础功要求 | 两类调研共同结论 |

短期先补 busy 和精神/行动资源，再补门派/师父数据模型。否则学习系统会继续显得像按钮升级。

### 7. 物品方向：先装备实例，再交易经济

当前物品还是 `ItemId -> amount`。这适合秘籍和证物，不适合装备、耐久、品质、掉落归属。

下一步应引入 item instance：

```text
ItemTemplate
  静态 YAML：名称、描述、类型、基础属性、使用效果

ItemInstance
  玩家/房间运行态：templateId、durability、quality、equippedSlot、owner/protection
```

优先级：

1. 装备槽和穿脱。
2. 装备属性进入 `DerivedStats`。
3. 战斗消耗耐久。
4. 修理。
5. 掉落表生成装备实例。
6. 交易/赠与/仓库。

不要先做交易市场。没有装备实例和事务边界，交易只会放大存档和一致性问题。

### 8. 心法方向：作为第二成长轴，但排在装备之后

《夺宝中华》的心法系统很有价值：它和武功分开，是可获得、可装备、可修炼、战斗中成长的 modifier。

但当前项目还没有装备实例和派生属性层。心法应在 `DerivedStats` 和装备系统之后做，否则会继续把加成塞进战斗代码。

建议模型：

```text
TechniqueTemplate
  kind: attack | internal | life | lightness | special
  requirements
  maxLevel
  growthRule
  statModifiers
  activeSkills

TechniqueInstance
  templateId
  level
  progress
  equippedSlot
  practiced
```

### 9. 房间方向：房间局部动作 + RoomObject

传统 MUD 的房间 `init()` / `valid_leave()` 和《夺宝中华》的 `map_things_*` 可以合并成两件事：

1. 房间配置层：
   - local actions。
   - enter/leave rules。
   - fight/steal/safe flags。
   - scene inspectables。

2. 房间运行态：
   - dropped money。
   - dropped item instance。
   - dropped technique。
   - corpse。
   - temporary NPC/job object。

这比继续把剧情掉落塞进玩家背包更接近 MUD。

### 10. 任务方向：剧情 quest 和循环 job 分开

当前 YAML quest 是 narrative quest，适合冷雨客栈这种章节。传统 MUD 和《夺宝中华》还需要另一类系统：可重复、随机目标、限时、按 streak 奖励的江湖任务。

建议拆成：

```text
QuestDefinition
  固定剧情、章节推进、一次性奖励

JobTemplate
  可重复任务模板：送信、追捕、寻物、巡查、打探

JobInstance
  玩家或世界运行态：目标、期限、进度、奖励倍率、seed
```

Job 可以复用 room action、RoomObject、NPC spawn，但不应污染剧情 quest 模型。

### 11. 持久化方向：JSON 可撑短期，但要先补迁移和位置

当前 JSON 存档足够支撑单机小规模玩法，但随着装备实例、心法实例、房间掉落、交易出现，会迅速变复杂。

短期必须补：

- 保存玩家当前位置。
- 显式 `migratePlayerSave`。
- 保存装备/心法实例前先设计 instance id。
- 明确 runtime state 不保存或只 checkpoint。

中期如果进入多人资产流转，优先引入 SQLite/Postgres，不要先引入 Redis 作为事实源。Redis 只适合作热状态、在线索引、消息队列和分布式锁。

## 收敛后的路线图

### Phase 1：收紧当前已落地系统

目标：把已有战斗、养成、存档从“能用”变成“边界清楚”。

- 给 `PlayerSave` 增加位置保存。
- 增加 `migratePlayerSave`。
- 客户端接入 `enable/prepare` 主动切换。
- 增加 action registry 的最小版本，用于返回当前可用动作。

### Phase 2：DerivedStats + 装备实例

目标：建立后续所有成长系统的底座。

- 新增 `DerivedStats` 计算函数。
- 新增 `ItemInstance`、装备槽、穿脱命令。
- 装备属性进入普通攻击和防御。
- 接入耐久消耗和修理。
- 存档保存装备实例。

### Phase 3：房间运行态和掉落表

目标：让世界有可拾取、会过期、可归属保护的临时对象。

- 新增 `RoomObject`。
- 剧情掉落改为生成 room object。
- 战斗掉落使用 drop table。
- 拾取检查位置、归属保护、负重/容量。
- tick 清理过期 room objects。

### Phase 4：busy、condition 和学习成本

目标：让养成动作进入时间维度。

- 新增 `busy` runtime state。
- `learn/practice/study/research/meditate` 进入 busy 或 consume action time。
- 增加精神/行动资源。
- 增加 `tickConditions`，统一战斗内外状态。
- 教师可教列表增加等级上限和资源成本。

### Phase 5：门派/师承与循环 job

目标：补传统 MUD 的长期玩法组织。

- `Faction` / `Teacher` YAML 模型。
- 拜师条件、叛门惩罚、师父教学上限。
- `JobTemplate` / `JobInstance`。
- 送信、追捕、寻物、打探等第一批 job。

### Phase 6：Technique/Xinfa 与数据库决策

目标：扩展第二成长轴，并决定持久化形态。

- 新增 Technique/Xinfa template 和 instance。
- 支持装备、修炼、战斗中成长。
- Technique modifier 进入 `DerivedStats`。
- 如果已有装备实例、房间对象、job、交易需求，迁移到 SQLite/Postgres。

## 不再作为近期目标的事项

- 不做 LPC 式任意脚本对象世界。
- 不做 Redis 事实源。
- 不做多人围攻，直到 encounter、奖励归属和房间对象稳定。
- 不做交易市场，直到 item instance 和事务边界稳定。
- 不做心法系统，直到 `DerivedStats` 和装备实例稳定。
- 不继续扩展散乱命令，先建立 action registry 和 busy/资源成本。

## 设计准则

1. 先建模，再做 UI。
2. 先单一权威状态，再做多人共享。
3. 先派生属性层，再加装备/心法/buff。
4. 先 runtime object，再做掉落和交易。
5. 先剧情 quest 和循环 job 分层，再扩展任务类型。
6. 先 JSON 迁移边界，再决定数据库。

这份路线的核心取舍是：吸收传统 MUD 和《夺宝中华》的玩法密度，但继续使用当前项目已经证明有效的强类型、集中状态、结构化协议和可测试 tick。
