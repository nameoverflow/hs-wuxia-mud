# 内容骨架规划

本文负责地图、NPC、任务和城市规模。剧情因果、玩家动机和威远旧线的回响原则以 [主线剧情大纲](./story-outline.md) 为准。

## 第一阶段规模

- 中心大城：`开封府`，古称汴梁。
- 序章地点：`开封城外破庙`、`汴水官道`、`威远分局`。
- 大城骨架：`洛阳`、`大理`。
- 小地点：`雁门旧道`、`太湖水榭`。
- 后续扩展：`少室山`、`武当山`、`华山道`与五岳诸山。

首阶段不追求传统 MUD 的数千房间规模，优先保证每条已开放路线都有明确的生活功能、人物关系和往返理由。

## 世界入口

序章“威远镖局旧案”只负责把玩家送进江湖：

```text
开封城外破庙
  -> 击退追兵
  -> 老镖师临终托付
  -> 沿汴水官道护送少年
  -> 抵达汴水南渡
  -> 威远分局前厅
  -> 少年独自离开
  -> 玩家从南门进入开封，开始自己的生活
```

序章完成后不自动开启威远调查任务。水浸路单作为远期暗线物件保留在玩家背包，少年去向只记录为剧情 flag。

## 开封府

开封承担第一阶段主要生活功能。官府与本地人称开封或府城，汴梁只作为古称与江湖旧称出现：

- 樊楼客舍：住宿和城市落点。
- 南门武馆、南门演武场：基础学习、试招和老师入口。
- 府学、铁铺、药铺：第一份工作及出身差异对白。
- 开封府捕房、驿馆与递铺：巡查、送信和追捕。
- 钱庄、当铺、茶楼、公告墙：经济、消息和后续社交入口。
- 鼓楼西侧的洛阳车马行、北街东侧转运区的大理商队和太湖药车：有叙事过程的远行交通。

玩家完成“开封落脚”后，应能自由选择练功、挣钱、巡查或远行，不需要继续围绕威远灭门案行动。

## 洛阳、大理与太湖

当前只提供区域骨架和老师 NPC，尚未实现正式 `Faction` 类型。

- 洛阳：名帖、酒会、剑法、轻功、白马寺和旧世家。
- 大理：商队、段氏旧府、茶马道、寺门和护送武学。段氏是前朝王族后裔，不掌当朝政权。
- 太湖：药车、水路、医者、调息和书信人情。

三处区域通过接引 NPC 进入。开封地图不再放置一步跨城的直接出口；玩家取得三城路引后，可以反复找对应接引人同行。

后续正式门派系统应再补：

```text
Faction YAML
  -> Teacher binding
  -> apprentice requirements
  -> contribution and reputation
  -> faction jobs
  -> detach consequences
```

## 雁门旧道

雁门旧道是独立的开封府差事区：

- 递铺亭触发追踪。
- 西侧小径出现玩家独立可见的探子。
- 击败探子后需要返回开封复命。
- 木牌只证明边报交接路径，不在开局阶段连接威远旧案。

后续可在剧情 quest 之外增加可重复送信、巡查和追捕 job。

## 当前已落资源

- `resources/scripts/maps/weiyuan_road.yaml`
- `resources/scripts/maps/bianshui_road.yaml`
- `resources/scripts/maps/kaifeng_city.yaml`
- `resources/scripts/maps/yanmen_old_road.yaml`
- `resources/scripts/maps/luoyang_city.yaml`
- `resources/scripts/maps/dali_city.yaml`
- `resources/scripts/maps/taihu_water_pavilion.yaml`
- `resources/scripts/quests/weiyuan_bloody_case.yaml`
- `resources/scripts/quests/content_skeleton_quests.yaml`
- `resources/scripts/characters/weiyuan_chapter.yaml`
- `resources/scripts/characters/content_skeleton_npcs.yaml`

## 后续制作顺序

1. 给洛阳、大理、太湖各补一条不依赖威远旧案的本地短任务。
2. 扩充开封住宿、买卖、治疗和工作行为。
3. 将雁门巡查拆出可重复 job 模板。
4. 等玩家拥有更长经历后，再设计威远旧线的第一次轻微回响。
