# 内容与剧情脚本

## 设计原则

当前内容系统采用 YAML 配置为主：

- 地图、NPC、武功、状态、物品、剧情都在 [resources/scripts/](../resources/scripts) 下定义。
- 剧情文本、实体名字、房间文案都属于内容脚本，不应放进 UI i18n。
- Haskell 提供少量通用动作和校验；内容作者通过 YAML 组合剧情流程。

## 资源目录

```text
resources/scripts/
  maps/          # 地图和房间
  characters/    # NPC
  martial_arts/  # 武功、普通攻击招式、主动招式
  effects/       # 状态效果
  items/         # 物品和可使用物品效果
  quests/        # 剧情事件
  default_player.yaml
```

加载入口是 [src/Game/World.hs](../src/Game/World.hs) 的 `loadAllAssets`。所有目录会被批量加载为 Map，key 来自实体 id。

## 地图

地图 YAML 定义：

- `id`, `name`, `desc`
- `rooms`
- room 的 `position`, `id`, `name`, `desc`, `exits`, `char`
- room 可选 `desc_variants`：按剧情 flag 替换房间描述，第一条命中的生效。用于让世界记得发生过的事，例如命案现场后来贴上封条：

```yaml
desc_variants:
  - flag: weiyuan.youth_separated
    desc: "门上交叉贴着开封府封条……"
```

方向使用：

- `north`, `south`, `east`, `west`
- `northeast`, `northwest`, `southeast`, `southwest`

出口支持两种写法。当前地图内移动可以直接写坐标：

```yaml
exits:
  north: [3, 4]
```

跨地图移动写目标地图和坐标：

```yaml
exits:
  north:
    map: bianshui_road
    position: [0, 0]
```

客户端会根据 server 返回的 `RoomExitSummary` 画出当前位置和出口节点，点击节点会直接移动。

## NPC

NPC YAML 定义：

- `id`, `name`, `desc`
- `actions`: `dialogue`, `attacking`, `sparring`
- `martial_arts` / `enabled`（旧写法 `prepared` 仍可读，并入 `enabled`）
- `dialogue`
- `teaches`: 可选，`{art_id: 可教上限}`。
- `teach_bonus`: 可选，师父每课额外熟练度，默认 0。
- `teach_requires`: 可选，`{flag, refusal}`。门派武学不传外人：玩家没有该剧情 flag 时，传授会被拒绝并显示 `refusal`。
- `rank`: 可选，覆盖按武功算出的档次，用于战斗奖励。
- `reward_exp_ceiling`: 可选，玩家实战经验达到该值后击败它不再有奖励（木人、木桩）。
- `reward`: 可选，`{combat_exp, potential}`，写死击败它的奖励，不走公式。
- `hidden`: 可选，`true` 表示新玩家初始不可见，需要剧情 `show_npc` 放出。
- `respawn`
- `attr`: `hp`, `qi`, `max_qi`, `qi_regen`, `str`, `agi`, `vit`

当前 UI 中 NPC 交互通过弹框完成：点击 NPC 后按其 `actions` 显示可用按钮。破庙黑衣人初始隐藏，由老镖师对话里的 `show_npc` 放出；战斗结束后再通过玩家故事状态隐藏。

## 剧情事件

剧情定义在 [resources/scripts/quests/](../resources/scripts/quests)。核心结构：

```yaml
id: weiyuan_bloody_case
name: "威远镖局旧案"
objectives:
  - stage: accepted
    text: "..."
reward:
  money: 80
  combat_exp: 20   # 可选，完成时发实战经验
  potential: 10    # 可选，完成时发潜能（受潜能软上限约束）
  items: []
events:
  - id: intro_wounded_escort
    trigger: ...
    conditions: ...
    actions: ...
```

### Trigger

当前支持：

- `talk`: 与指定 NPC 对话。
- `enter_room`: 进入指定地图坐标。
- `kill`: 杀死指定 NPC 后触发。

### Condition

当前支持：

- `quest_not_started`
- `quest_stage`
- `quest_completed`
- `flag`
- `not_flag`
- `npc_dead`
- `npc_alive`
- `has_item` / `not_has_item`: 玩家背包里是否有指定物品，字段 `item`。
- `in_map`: 玩家当前是否在指定地图，字段 `map`。同一 NPC 出现在多张地图时，用它区分去程和回程。

### Action

当前支持：

- `message`: 发送剧情文本；若后面没有显式 `delay`，server 会按正文长度补阅读停顿。
- `delay`: 由 server 输出队列暂停该玩家的后续消息，字段 `ms`。
- `transition`: 地图区域转场，字段 `text` / `ms`。
- `set_stage`
- `complete_quest`
- `set_flag`
- `clear_flag`
- `hide_npc`
- `show_npc`
- `give_item`
- `give_money`
- `learn_art`
- `start_battle`
- `move_player`

`learn_art` 仍作为通用剧情动作存在，但当前威远镖局旧案设计中不直接用它教玩家武功；玩家后续通过秘籍物品学习武功。

## 线性剧情推进

剧情事件不再向客户端发送选择。需要推进剧情时，把后续动作直接排在 `message` 后面：

```yaml
actions:
  - type: message
    speaker: "受伤老镖师"
    text: "别点火。后面有人追着我们镖局的车来，庙里一亮，暴露了我们都凶多吉少。"
  - type: set_stage
    quest: weiyuan_bloody_case
    stage: intruder
  - type: message
    speaker: "旁白"
    text: "庙门外响起急促脚步。有人一脚踹开半扇破门，雨水和冷风一起灌进殿里。"
  - type: show_npc
    npc: temple_black_clad
```

server 会按 action 顺序生成并调度玩家的剧情输出。`message`、NPC 显隐、场景刷新和奖励都保持脚本顺序；场景内获得的多项奖励会合并到该段剧情结束后发送。
`delay` 在 server 端真正延后后续消息，文本客户端与图形客户端会得到相同节奏。`transition` 会先发送转场提示；紧接 `move_player` 时，server 会在遮罩进入后发送新房间，再等转场完整结束才发送下一句剧情。剧情输出未完成前，同一连接收到的后续指令会按 MUD 命令队列顺序等待。

## 物品使用脚本

物品定义支持可选 `use`：

```yaml
id: weiyuan_sword_manual
name: "威远剑谱"
desc: "威远镖局的入门剑谱，记着押镖护身常用的基础步法和几式剑招。"
use:
  type: learn_art
  art: weiyuan_sword
  level: 1
  min_level: 1     # study 推进熟练度的等级带，可选
  max_level: 10    # 读到这一级后再研读没有收获，可选，默认武功上限
  quality: 2       # 每次研读额外熟练度 2 * quality，可选
  consume: false
  message: "..."
  repeat_message: "..."
```

当前 `use.type` 只支持 `learn_art`：

- 首次使用：学习配置的武功；该类型还没有启用武功时自动启用。
- `study` 同一本秘籍：未学会时学会，已学会时在等级带内推进熟练度。
- 重复使用：如果玩家已学到同等级或更高等级，只显示 `repeat_message`，不重复发武功奖励。
- `consume: false` 表示秘籍使用后保留在背包。

这条路径对应当前设计：剧情奖励给“秘籍物品”，玩家主动使用后才学会武功。

## 武功脚本

武功定义在 [resources/scripts/martial_arts/](../resources/scripts/martial_arts)。文件可以是一门武功，也可以是武功列表。

基础功：

```yaml
id: basic_sword
name: "基础剑法"
type: foundation
desc: "一切剑法的起手。"
max_level: 100
```

具体武功：

```yaml
id: weiyuan_sword
name: "威远剑法"
type: sword
desc: "..."
foundation: basic_sword
tier: entry          # entry | regional | lineage；非 entry 必须写大于 1 的基础功门槛
requires:
  - art: basic_sword
    level: 1
max_level: 20
research_from: 10    # 可选，默认 max_level 的一半向上取整
attack_moves:
  - id: weiyuan_slash
    name: "护镖横斩"
    unlock_level: 1
    desc: "..."
    msg: "..."
    damage: 14
active_skills:
  - id: eight_direction_thrusts
    name: "八方连刺"
    unlock_level: 5
    desc: "..."
    msg: "..."
    cd: 20.0
    target: Single
    cost: 80
    ap_req: 100
    req_status: []
    damage: 120
    effect:
      self: []
      target: []
```

约定：

- 武功类型固定为 `foundation/internal/lightness/sword/fist`。
- 基础功由 `default_player.yaml` 授予，不能直接训练。
- 秘籍或剧情 `learn_art` 只授予武功；是否满足 `requires` 由 server 检查。
- `unlock_level` 不写时默认 1。

## 世界校验

`validateWorld` 当前会校验：

- 地图房间引用的 NPC 是否存在。
- 物品 `use.learn_art` 引用的武功是否存在，等级是否为正。
- 剧情 trigger/condition/action 引用的 quest、room、NPC、item、martial art 是否存在。
- quest reward item 是否存在且数量为正。
- objective stage 是否为空。
- 武功 `foundation` 和 `requires` 是否引用存在武功。
- 武功 `max_level`、`requires.level`、`attack_moves.unlock_level`、`active_skills.unlock_level` 是否为正。
- 学习奖励等级和招式解锁等级是否超过目标武功 `max_level`。
- `research_from` 是否在 `1..max_level` 内；非 `entry` 阶武功是否有大于 1 的基础功门槛。
- 秘籍等级带 `min_level <= max_level`，且不超过武功上限。

这能在 server 启动阶段尽早暴露配置错误。

## 威远镖局旧案章节

剧情因果以 [主线剧情大纲](./story-outline.md) 为准，这里只记录脚本结构：

1. 与老镖师对话：`set_stage intruder`，旁白闯门，`show_npc` 放出隐藏的黑衣人。
2. 与黑衣人对话：`start_battle` 开启玩家独立剧情战斗，不占用全局 NPC 战斗锁。
3. `kill` 黑衣人：隐藏黑衣人，设置 `weiyuan.saw_ink_mark`，进入 `escort_dying`。
4. 再与老镖师对话：给盘缠和水浸路单，老镖师断气并隐藏，`transition` + `move_player` 送到汴水官道。
5. `enter_room` 汴水南渡进入 `branch_gate`；`enter_room` 前厅时少年交出剑谱、隐藏，进入 `inner_court`。
6. `enter_room` 内院：井台留言，设置分离 flag，完成本章并开启 `first_steps_kaifeng`，再转场到柳岸码头。
7. 前厅、门口、内院通过 `desc_variants` 在 `weiyuan.youth_separated` 后显示封条现场。

序章之后的 `first_steps_kaifeng` 属于玩家自己的住宿、试招和谋生流程；城中风声在 `kaifeng_echoes`，没有 objectives，因此不出现在任务栏。
