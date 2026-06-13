# 公开武侠 MUD 服务端源码调研

本文记录对若干公开中文 MUD 服务端/mudlib 源码的调研结论，并对照当前 Haskell 实现，提炼可迁移的玩法和架构设计。

调研重点不是直接移植 LPC 代码，而是理解传统武侠 MUD 如何组织世界、命令、战斗、武功、师承和任务系统。

## 调研对象

| 项目 | 类型 | 主要价值 | 备注 |
| --- | --- | --- | --- |
| [MudRen/xkx100](https://github.com/MudRen/xkx100) | 侠客行 100 UTF-8 mudlib | 观察侠客行系目录结构、WebSocket 端口、传统资源组织 | 授权状态需要单独确认 |
| [oiuv/mud](https://github.com/oiuv/mud) | 炎黄 MUD / 侠客行类 mudlib | 代码较完整，适合分析命令、战斗、技能、任务、门派 | README 声明 MIT |
| [mudchina/es2-utf8](https://github.com/mudchina/es2-utf8) | ES2 UTF-8 mudlib | 代表中文 LPMUD 的早期基础模型 | README 声明 MIT |
| [fluffos/fluffos](https://github.com/fluffos/fluffos) | LPMUD driver | 解释传统 mudlib 的运行时边界 | C++ driver，非玩法代码 |

未确认存在可直接使用的“北大侠客行”原始服务端开源仓库。公开 GitHub 上能看到一些 PKU 侠客行相关仓库，但多为客户端、bot 或资料工具。服务端设计参考应以 xkx100、yhmud、es2 这类公开 mudlib 为主。

## 总体架构

传统中文武侠 MUD 大多是 LPMUD 体系：

```text
FluffOS / MudOS driver
  -> LPC mudlib
      -> cmds/
      -> d/
      -> clone/
      -> kungfu/
      -> inherit/ feature/ std/
      -> adm/daemons/
```

Driver 负责网络、对象生命周期、LPC 解释执行、心跳和底层运行时。Mudlib 负责全部游戏逻辑和内容。

典型目录含义：

- `cmds/`：玩家命令。每个命令通常是一个 LPC 文件，例如 `go`、`look`、`learn`、`practice`、`perform`。
- `d/`：地图区域。每个房间通常是一个对象文件。
- `clone/`：可复制对象，包括 NPC、装备、物品、任务物品、动态任务 NPC。
- `kungfu/skill/`：武功文件。每门武功可以同时包含普通招式、战斗修正、绝招、内功功能。
- `inherit/`、`feature/`、`std/`：角色、房间、物品等基类和可组合能力。
- `adm/daemons/`：全局系统对象，例如战斗、任务、NPC 生成、玩家数据更新。

这个模型的核心是“对象世界”：房间、NPC、武功、任务都不是纯配置，而是可执行对象。它非常灵活，但内容和逻辑高度耦合。

## 命令分发

yhmud 的命令分发入口可以参考：

- [feature/command.c](https://github.com/oiuv/mud/blob/master/feature/command.c)
- [cmds/std/go.c](https://github.com/oiuv/mud/blob/master/cmds/std/go.c)

典型流程：

1. 玩家输入一行文本。
2. 解析 verb。
3. 如果 verb 是当前房间出口方向，转为移动命令。
4. 在玩家命令路径中查找对应命令文件。
5. 若没有找到，继续尝试 emote、频道、自动说话、房间局部动作。
6. 执行命令对象中的 `main()` 或房间注册的 action 函数。

房间可以在 `init()` 中注册局部命令。例如进入、攀爬、推动机关、打开暗门等，都可以作为房间自己的动作。

对当前项目的启发：

- 现在的 JSON action 更像固定 API。后续需要一个更通用的 command/action registry。
- 房间需要支持局部动作，例如 `on_interact`、`valid_leave`、`on_enter`。
- 移动不应只是改坐标，还应经过负重、busy、战斗逃跑、守卫阻拦、特殊出口等规则。

## 地图与房间

典型房间文件可以参考：

- [inherit/room/room.c](https://github.com/oiuv/mud/blob/master/inherit/room/room.c)
- [d/city/guangchang.c](https://github.com/oiuv/mud/blob/master/d/city/guangchang.c)

房间通常声明：

- `short`：短标题。
- `long`：长描述。
- `exits`：出口映射。
- `objects`：房间刷新对象。
- `item_desc`：可查看的场景细节。
- `no_fight`、`no_steal` 等规则 flag。
- `init()`：玩家进入房间后注册特殊动作。
- `valid_leave()`：控制是否允许离开。

当前项目的 YAML 地图已覆盖房间描述、出口和 NPC 放置，但还缺：

- 房间局部动作。
- 房间规则 flag。
- 房间进入/离开 hook。
- 场景物件查看。
- 自动刷新对象数量控制。

## 角色与心跳

角色基类可以参考：

- [inherit/char/char.c](https://github.com/oiuv/mud/blob/master/inherit/char/char.c)

传统 MUD 的角色对象继承大量 feature：

- command：命令执行。
- move：移动。
- attack：战斗目标和攻击。
- skill：技能、映射、准备。
- condition：中毒、状态、时间效果。
- finance：金钱。
- team：组队。
- save：玩家存档。

角色 `heart_beat()` 中会处理：

- busy 和 continue action。
- 濒死、昏迷、恢复。
- 自动逃跑。
- 战斗中调用 `attack()`。
- NPC 扫描周围环境。
- condition tick。
- 饥渴、idle、自动保存。

当前项目使用全局 game tick 统一推进状态。这个方向更容易测试和控制并发，但可以借鉴传统 MUD 的 actor tick 拆分方式：

- `tickBattle`
- `tickNpc`
- `tickCondition`
- `tickRespawn`
- `tickAutoSave`
- `tickWorldEvent`

## 战斗实现

yhmud 的战斗核心可以参考：

- [adm/daemons/combatd.c](https://github.com/oiuv/mud/blob/master/adm/daemons/combatd.c)
- [feature/attack.c](https://github.com/oiuv/mud/blob/master/feature/attack.c)

传统战斗不是当前项目这种显式 `Battle` 对象，而是角色心跳驱动：

```text
char.heart_beat
  -> attack()
      -> COMBAT_D->fight(attacker, victim)
          -> COMBAT_D->do_attack(attacker, victim, weapon, attack_type)
```

一次普通攻击大致包含：

1. 根据武器或准备的拳脚技能确定攻击类型。
2. 根据已映射武功选择当前招式。
3. 计算攻击 AP。
4. 计算闪避 DP 并判定是否躲开。
5. 计算招架 PP 并判定是否招架。
6. 计算基础伤害、武器伤害、臂力、加力、内功影响。
7. 调用技能、武器、护甲、目标对象上的 hook。
8. 造成气血/精/内力等伤害，输出战斗文本。

这套系统的关键不是公式本身，而是扩展点：

- 武功可以提供 `query_action()` 生成招式。
- 武功可以提供 `hit_ob()` 扩展命中效果。
- 防御类武功可以提供 `valid_damage()` 改写伤害。
- 武器和护甲也能参与伤害流程。

当前项目已经有 AP、Qi、cooldown、effect，结构更现代。但普通攻击仍偏简单。建议后续把普通攻击扩展为：

```text
attacker prepared art
  -> pick unlocked attack move
  -> hit/dodge/parry/armor calculation
  -> art/equipment/status hooks
  -> damage/effects/messages
```

## 主动绝招与内功功能

主动绝招命令可以参考：

- [cmds/skill/perform.c](https://github.com/oiuv/mud/blob/master/cmds/skill/perform.c)
- [kungfu/skill/xianglong-zhang/fei.c](https://github.com/oiuv/mud/blob/master/kungfu/skill/xianglong-zhang/fei.c)

内功功能可以参考：

- [cmds/skill/exert.c](https://github.com/oiuv/mud/blob/master/cmds/skill/exert.c)
- [kungfu/skill/force/recover.c](https://github.com/oiuv/mud/blob/master/kungfu/skill/force/recover.c)

传统 MUD 将主动能力分为两类：

- `perform`：武功绝招，通常依赖拳脚、兵器、轻功等。
- `exert`：内功功能，例如疗伤、护体、回气、爆发。

绝招通常检查：

- 是否在战斗。
- 是否装备了正确武器。
- 是否映射或准备了正确武功。
- 武功等级是否足够。
- 内力、精力、气血是否足够。
- 是否 busy。
- 是否满足门派或前置技能条件。

当前项目的 `active_skills` 已经覆盖 cost、AP requirement、cooldown、status requirement、damage/effect。后续可以扩展：

- `requires_art`
- `requires_prepared`
- `requires_weapon_type`
- `requires_attr`
- `requires_resource`
- `busy_self`
- `busy_target`
- `multi_hit`
- `on_hit_effect`

## 技能与养成

yhmud 的技能状态可以参考：

- [feature/skill.c](https://github.com/oiuv/mud/blob/master/feature/skill.c)
- [cmds/skill/learn.c](https://github.com/oiuv/mud/blob/master/cmds/skill/learn.c)
- [cmds/skill/practice.c](https://github.com/oiuv/mud/blob/master/cmds/skill/practice.c)
- [cmds/skill/enable.c](https://github.com/oiuv/mud/blob/master/cmds/skill/enable.c)
- [cmds/skill/prepare.c](https://github.com/oiuv/mud/blob/master/cmds/skill/prepare.c)
- [cmds/skill/study.c](https://github.com/oiuv/mud/blob/master/cmds/skill/study.c)
- [cmds/skill/exercise.c](https://github.com/oiuv/mud/blob/master/cmds/skill/exercise.c)
- [cmds/skill/respirate.c](https://github.com/oiuv/mud/blob/master/cmds/skill/respirate.c)
- [cmds/skill/research.c](https://github.com/oiuv/mud/blob/master/cmds/skill/research.c)

传统武侠 MUD 的养成层通常包含：

- `skills`：已学技能等级。
- `learned`：潜能学习进度。
- `skill_map`：把基础类别映射到特殊武功，例如 `force -> huntian-qigong`。
- `skill_prepare`：准备拳脚类技能，例如掌法、指法、爪法。
- `enable`：启用一门特殊武功到基础类别。
- `prepare`：准备拳脚技能用于出招。
- `learn`：向师父学习，消耗潜能和精。
- `practice`：练习技能，通常受基础技能上限约束。
- `study`：读书学习，依赖 literate 和书籍条件。
- `exercise` / `dazuo`：打坐，将气转为内力或内力上限。
- `respirate` / `tuna`：吐纳，将精转为精力或精力上限。
- `research`：高等级自研，消耗潜能和精。

这与当前项目的差距较大。当前项目已经有：

- 基础功和具体武功。
- 武功等级。
- 学习门槛。
- 招式解锁。
- 训练升级。
- 准备武功。

还缺：

- 潜能或熟练度资源。
- 师父教学。
- `enable` 和 `prepare` 的显式玩家操作。
- 读书、打坐、吐纳、自研。
- 内力上限、精力上限等长期资源成长。
- 基于 combat experience 的技能上限。

建议保留当前“无角色等级，成长落在武功上”的方向，但把养成动作拆细：

```text
learn    从师父/秘籍获得武功或提高入门等级
practice 消耗时间/资源提升熟练度
enable   将特殊武功映射到基础类别
prepare  选择拳脚/兵器战斗出招
study    通过书籍推进技能
meditate 打坐提升内力相关资源
```

## 门派与师父

典型师父 NPC 可以参考：

- [kungfu/class/gaibang/hong.c](https://github.com/oiuv/mud/blob/master/kungfu/class/gaibang/hong.c)

传统 MUD 中，门派和师父逻辑大量写在 NPC 文件中：

- NPC 基础属性和装备。
- NPC 自己掌握的技能。
- `map_skill` 和 `prepare_skill`。
- `create_family` 门派身份。
- `attempt_apprentice()` 拜师条件。
- `recognize_apprentice()` 临时认可。
- `no_teach` 禁教技能。
- `chat_msg_combat` 战斗 AI。
- `inquiry` 打听信息。

这种做法灵活，但强耦合。当前项目更适合拆成：

- `Faction` 配置：门派 id、名称、辈分、入门条件。
- `Teacher` 配置：可教技能、上限、拜师条件、禁教列表。
- NPC AI 配置：战斗中可用 active skill、触发条件、权重。
- 少量脚本 hook：处理特殊拜师剧情和例外条件。

## 任务系统

yhmud 的任务系统可以参考：

- [adm/daemons/questd.c](https://github.com/oiuv/mud/blob/master/adm/daemons/questd.c)
- [inherit/misc/quest.c](https://github.com/oiuv/mud/blob/master/inherit/misc/quest.c)
- [clone/quest/deliver.c](https://github.com/oiuv/mud/blob/master/clone/quest/deliver.c)
- [clone/quest/capture.c](https://github.com/oiuv/mud/blob/master/clone/quest/capture.c)
- [inherit/item/task.c](https://github.com/oiuv/mud/blob/master/inherit/item/task.c)

传统任务分两类：

1. 师门或系统任务。
   - 由 `QUEST_D` 管理。
   - 状态写在玩家 `quest/*` 字段。
   - 常见类型包括送信、杀人、巡查、寻找。
   - 奖励经验、潜能、金钱、门派贡献或计数奖励。

2. 动态江湖任务。
   - 任务对象运行时生成 NPC、物品、地点和线索。
   - 任务可以注册“打听信息”。
   - 有生命周期，过期后自动销毁。
   - 例如送物、追捕、寻人、夺回物品。

当前项目的 quest 更接近 narrative quest：通过 YAML 触发剧情事件、推进 stage、给奖励。后续建议把任务分成三层：

```text
QuestDefinition       固定剧情任务
JobTemplate           可重复任务模板
QuestRuntimeInstance  某个玩家或世界中的运行时任务实例
```

适合优先实现的动态任务模板：

- `deliver`：送信或送物。
- `capture`：追捕随机 NPC。
- `retrieve`：找回物品。
- `escort`：护送 NPC。
- `rumor`：通过打听信息寻找目标。

## 存档与数据修正

玩家存档可以参考：

- [clone/user/user.c](https://github.com/oiuv/mud/blob/master/clone/user/user.c)
- [adm/daemons/updated.c](https://github.com/oiuv/mud/blob/master/adm/daemons/updated.c)

传统 MUD 玩家对象直接保存大量字段：

- 当前位置。
- 基础属性。
- 气血、精、内力、精力。
- 技能、映射、准备。
- 门派、师承、称号。
- 背包和自动加载物品。
- 任务状态。
- 钱庄、仓库、社交关系。

并且常有一个全局更新 daemon 修正老玩家数据，例如字段迁移、上限修正、非法技能清理。

当前项目现在只有 JSON 玩家存档，已保存 story、inventory、money、arts、prepared 等核心字段。后续如果继续 JSON，也建议增加版本字段：

```json
{
  "saveVersion": 1,
  "player": {}
}
```

并提供 `migratePlayerSave`，集中处理存档升级。

## 与当前 Haskell 实现对照

| 维度 | 传统 LPC mudlib | 当前 Haskell 项目 | 建议 |
| --- | --- | --- | --- |
| 运行时 | FluffOS/MudOS driver | Haskell WebSocket server | 保持当前强类型 server |
| 内容 | LPC 对象即内容 | YAML 配置 | 继续 YAML，但给复杂内容留 hook |
| 状态 | 对象自身持有状态 | 集中 `GameState` | 保持集中状态，按系统拆 tick |
| 命令 | 文本命令动态分发 | JSON action | 建 command/action registry |
| 房间 | 可执行对象，局部 action | 静态 YAML 房间 | 增加局部动作和房间规则 |
| 战斗 | 心跳驱动，公式和 hook 丰富 | 显式 Battle，AP/Qi/effect | 保留 Battle，补命中/闪避/招架/装备/技能 hook |
| 技能 | learn/practice/enable/prepare/study/dazuo/tuna | 学习、训练、准备、招式解锁 | 引入潜能、师父、读书、打坐、吐纳 |
| 任务 | 固定任务 + 动态任务对象 | YAML 剧情事件 | 增加 job template 和 runtime instance |
| 存档 | 玩家对象持久化，多字段 | JSON 玩家存档 | 加 saveVersion 和迁移层 |

## 可落地的近期设计

优先级从低风险到高影响：

1. 增加房间局部动作模型。
   - YAML 增加 `actions` 或 `interactions`。
   - 支持查看场景物件、打开入口、触发剧情、移动到特殊房间。

2. 增加显式准备/切换武功。
   - server action: `prepare_art`。
   - client 武学面板提供切换。
   - 战斗普通攻击只使用当前 prepared art。

3. 扩展普通攻击公式。
   - 命中、闪避、招架、防御。
   - 角色属性和武功等级参与计算。
   - 装备预留接口。

4. 增加师父和学习规则。
   - NPC 可声明 `teacher`。
   - 武功可声明 teacher-only。
   - 学习消耗潜能或熟练度资源。

5. 引入动态任务模板。
   - 先实现 `deliver` 和 `capture`。
   - 运行时生成目标 NPC、目标地点、过期时间。
   - 玩家存档只保存任务实例摘要。

6. 增加存档版本迁移。
   - 保存 `saveVersion`。
   - 启动或读取时自动迁移旧存档。

## 设计取舍

不建议直接复制传统 LPC mudlib 的组织方式：

- 全局 daemon 和字符串路径过多，强类型项目中会降低可维护性。
- 内容对象直接执行任意逻辑，测试成本高。
- 房间、NPC、任务、武功互相引用自由，容易形成隐式依赖。
- 历史 mudlib 常有大量兼容旧数据的特殊分支。

建议吸收的是玩法层结构：

- 命令分发的开放性。
- 房间局部动作。
- 武功映射和准备。
- 师父教学和门派约束。
- 读书、打坐、吐纳、自研等养成动作。
- 普通攻击公式和技能 hook。
- 动态江湖任务。

当前项目应继续走“强类型核心 + YAML 内容 + 有限 hook”的路线。复杂玩法先抽象成 Haskell 数据结构和系统函数，只有在 YAML 表达能力明显不够时，再考虑脚本 hook。
