# hs-wuxia-mud 文档索引

本文档集记录当前实现状态和后续设计边界。项目仍处于 WIP，文档描述的是当前代码快照，而不是最终目标。

## 文档目录

- [运行与开发](./running.md)：本地启动 server/client、测试命令、常见输出。
- [系统架构](./architecture.md)：后端模块分层、状态流、线程模型。
- [内容与剧情脚本](./content-scripting.md)：YAML 资源格式、剧情事件、物品使用、冷雨客栈章节。
- [战斗、武功与物品](./gameplay-systems.md)：AP/Qi 战斗、技能、秘籍学习、奖励与掉落。
- [客户端 UI](./client-ui.md)：WebSocket 客户端、方位图、NPC 弹框、i18n 边界。
- [协议与存档](./protocol-and-persistence.md)：网络事件、响应消息、玩家存档字段。
- [状态与路线图](./status-and-roadmap.md)：已完成能力、已知缺口、高优先级下一步。

## 当前一句话概括

这是一个 Haskell WebSocket MUD：后端以 YAML 加载地图、NPC、武功、物品和剧情事件，客户端提供武侠风网页 UI。当前可玩闭环包含移动、查看房间、NPC 交互、剧情选择、战斗、剧情完成、掉落物、秘籍物品、使用秘籍学习武功、背包/任务显示和基础玩家存档。

## 关键源码入口

- Server 启动：[app/Main.hs](../app/Main.hs)
- WebSocket 与存档调度：[src/Server.hs](../src/Server.hs)
- 游戏主逻辑：[src/GamePlay.hs](../src/GamePlay.hs)
- 全局状态 Monad：[src/GameState.hs](../src/GameState.hs)
- 战斗子系统：[src/Game/Combat.hs](../src/Game/Combat.hs)
- 实体定义与 YAML 解析：[src/Game/Entity.hs](../src/Game/Entity.hs)
- 剧情脚本模型：[src/Game/Quest.hs](../src/Game/Quest.hs)
- 世界加载与校验：[src/Game/World.hs](../src/Game/World.hs)
- 网络消息：[src/Game/Message.hs](../src/Game/Message.hs), [src/Networking.hs](../src/Networking.hs)
- 玩家存档：[src/Database.hs](../src/Database.hs)
- Web 客户端：[client/](../client)
- 内容脚本：[resources/scripts/](../resources/scripts)
- 测试：[test/Spec.hs](../test/Spec.hs)
