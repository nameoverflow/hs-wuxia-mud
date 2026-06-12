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
  skills/        # 武功、普通招式、主动技能
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

方向使用：

- `north`, `south`, `east`, `west`
- `northeast`, `northwest`, `southeast`, `southwest`

客户端会根据 server 返回的 `RoomExitSummary` 画出当前位置和出口节点，点击节点会直接移动。

## NPC

NPC YAML 定义：

- `id`, `name`, `desc`
- `actions`: `dialogue`, `attacking`, `sparring`
- `skills` / `prepared`
- `dialogue`
- `respawn`
- `attr`: `hp`, `qi`, `max_qi`, `qi_regen`, `str`, `agi`, `vit`

当前 UI 中 NPC 交互通过弹框完成：点击 NPC 后按其 `actions` 显示可用按钮。剧情完成后的青衣客只保留 `dialogue`，纸伞客则通过玩家故事状态隐藏。

## 剧情事件

剧情定义在 [resources/scripts/quests/](../resources/scripts/quests)。核心结构：

```yaml
id: cold_rain_inn
name: "冷雨客栈"
objectives:
  - stage: accepted
    text: "..."
reward:
  money: 80
  items: []
events:
  - id: intro_innkeeper
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

### Action

当前支持：

- `message`: 发送剧情文本，可带 choices。
- `set_stage`
- `complete_quest`
- `set_flag`
- `clear_flag`
- `hide_npc`
- `give_item`
- `give_money`
- `learn_art`
- `start_battle`

`learn_art` 仍作为通用剧情动作存在，但当前冷雨客栈设计中不直接用它教玩家武功；青衣客只给秘籍，真正学习发生在玩家使用秘籍物品时。

## 选择

`message` 可带 `choices`：

```yaml
choices:
  - id: cold_rain_accept_wine
    text: "接过冷酒，进客栈大堂"
    actions:
      - type: set_stage
        quest: cold_rain_inn
        stage: accepted
```

server 会把 choice id 和文本发给客户端。客户端点击后发送：

```json
{"choose":"cold_rain_accept_wine"}
```

server 从 `storyPendingChoices` 中取出对应 action 执行，并清空 pending choices。

## 物品使用脚本

物品定义支持可选 `use`：

```yaml
id: cold_rain_manual
name: "听雨残谱"
desc: "薄薄几页纸，没有招式图。只有几行字：先听雨，再出手。"
use:
  type: learn_art
  art: cold_rain_secret
  level: 1
  consume: false
  message: "..."
  repeat_message: "..."
```

当前 `use.type` 只支持 `learn_art`：

- 首次使用：学习配置的武功并自动准备该武功。
- 重复使用：如果玩家已学到同等级或更高等级，只显示 `repeat_message`，不重复发武功奖励。
- `consume: false` 表示秘籍使用后保留在背包。

这条路径对应当前设计：剧情奖励给“秘籍物品”，玩家主动使用后才学会技能。

## 世界校验

`validateWorld` 当前会校验：

- 地图房间引用的 NPC 是否存在。
- 物品 `use.learn_art` 引用的武功是否存在，等级是否为正。
- 剧情 trigger/condition/action 引用的 quest、room、NPC、item、martial art 是否存在。
- quest reward item 是否存在且数量为正。
- objective stage 是否为空。

这能在 server 启动阶段尽早暴露配置错误。

## 冷雨客栈章节

当前唯一完整章节是 `cold_rain_inn`：

1. 玩家在渡口与冷雨掌柜对话，接冷酒。
2. 去大堂见青衣客，选择留下见证。
3. 到后院天井看见纸伞客。
4. 与纸伞客对话，选择打翻灯笼，进入战斗。
5. 杀死纸伞客后：
   - 完成章节。
   - 玩家获得 `80` 铜钱。
   - 纸伞客对该玩家隐藏。
   - 掉落 `伞骨银针`。
6. 回大堂与青衣客对话：
   - 首次给 `听雨残谱`。
   - 后续只给固定回应。
7. 使用 `听雨残谱` 后学习并准备 `cold_rain_secret`。
