# hs-wuxia-mud 文档索引

本文档集记录当前实现状态和后续设计边界。项目仍处于 WIP，文档描述的是当前代码快照，而不是最终目标。

## 文档目录

- [运行与开发](./running.md)：本地启动 server/client、测试命令、常见输出。
- [系统架构](./architecture.md)：后端模块分层、状态流、线程模型。
- [Tick 循环、心跳与战斗推进](./tick-loop-and-heartbeat.md)：server tick phase、dirty 保存策略、战斗流水线和 busy 规划。
- [MUD 调研综合结论与落地路线](./mud-design-synthesis.md)：整合公开 mudlib、《夺宝中华》和当前已落地功能后的统一设计判断。
- [内容与剧情脚本](./content-scripting.md)：YAML 资源格式、剧情事件、物品使用、冷雨客栈章节。
- [战斗、武功与物品](./gameplay-systems.md)：AP/Qi 战斗、主动招式、秘籍学习、奖励与掉落。
- [角色养成系统设计](./character-progression.md)：无角色等级、基础功门槛、武功升级、招式解锁。
- [公开武侠 MUD 服务端源码调研](./public-mud-source-study.md)：xkx100、yhmud、es2 等公开 mudlib 的调研细节。
- [《夺宝中华》WAP MUD 调研](./duobao-zhonghua-study.md)：玩法/养成建模、Redis/MySQL 同步和持久化细节。
- [客户端 UI](./client-ui.md)：WebSocket 客户端、方位图、NPC 弹框、i18n 边界。
- [协议与存档](./protocol-and-persistence.md)：网络事件、响应消息、玩家存档字段。
- [状态与路线图](./status-and-roadmap.md)：当前能力快照、已知缺口和综合路线的近期执行顺序。

## 当前一句话概括

这是一个 Haskell WebSocket MUD：后端以 YAML 加载地图、NPC、武功、物品和剧情事件，客户端提供武侠风网页 UI。当前可玩闭环包含移动、查看房间、NPC 交互、线性剧情推进、战斗、剧情完成、掉落物、秘籍物品、使用秘籍学习武功、背包/任务显示和基础玩家存档。本地开发支持 `MUD_DEV_MODE=1` + `?test=1` 自动登录并重置测试角色。

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
