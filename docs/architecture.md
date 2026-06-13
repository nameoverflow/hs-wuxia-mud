# 系统架构

## 模块分层

```text
app/Main.hs
  -> Server.hs
      -> Networking.hs / Game.Message
      -> GamePlay.hs
          -> GameState.hs
          -> Game.World
          -> Game.Combat
          -> Game.Quest
          -> Game.Entity
      -> Database.hs
client/
resources/scripts/
```

## 后端职责

- [app/Main.hs](../app/Main.hs)：加载 `resources/scripts`，创建 `MVar GameState`，启动 game tick 线程和 WebSocket server。
- [src/Server.hs](../src/Server.hs)：处理登录、dev-mode 测试重置、连接表、消息循环、响应分发、自动保存。
- [src/GameState.hs](../src/GameState.hs)：定义全局 `GameState` 和主游戏 Monad。
- [src/GamePlay.hs](../src/GamePlay.hs)：玩家动作、移动、查看、对话、剧情、物品使用、主动招式施展、战斗结算。
- [src/Game/Combat.hs](../src/Game/Combat.hs)：战斗内部状态、AP/Qi tick、普通攻击、主动招式、状态效果。
- [src/Game/World.hs](../src/Game/World.hs)：加载 YAML 内容，构造 `World`，做跨资源引用校验。
- [src/Database.hs](../src/Database.hs)：JSON 玩家存档。

## 主状态

`GameState` 当前包含：

- `world`：静态内容加少量运行态 NPC 状态。
- `players`：在线或已创建玩家。
- `battles`：按玩家 id 存储的一对一战斗。
- `respawn`：NPC 复活倒计时。
- `stories`：玩家个人剧情状态。

`World` 当前包含：

- `items`
- `maps`
- `chars`
- `effects`
- `quests`
- `martialArts`

注意：`world.chars.charStatus` 是全局 NPC 状态；剧情中的 `storyHiddenNpcs` 是每个玩家自己的可见性状态。剧情人物完成后消失主要靠 `HideNpc` 写入玩家故事状态，而不是只依赖全局死亡状态。

## Monad 结构

主游戏 Monad：

```haskell
WriterT [PlayerResp] (StateT GameState (ExceptT GameException IO))
```

职责：

- `WriterT` 收集要发给客户端的响应。
- `StateT` 修改 `GameState`。
- `ExceptT` 返回游戏错误。
- `IO` 用于随机数、读写配置、server 侧副作用。

战斗 Monad：

```haskell
RWST World [PlayerResp] Battle (ExceptT CombatException (Rand StdGen))
```

职责：

- `Reader` 读取不可变世界配置。
- `Writer` 输出战斗消息。
- `State` 修改单场 `Battle`。
- `Except` 返回战斗错误。
- `Rand` 做普通攻击招式随机选择。

## 动作流

客户端发送 `NetPlayerAction`：

```text
WebSocket
  -> Server.runGameLoop
  -> GamePlay.processPlayerAction
  -> GameStateT 修改状态并写出 PlayerResp
  -> Server.dispatchResp
  -> Svelte client state/components 渲染
```

game tick：

```text
Server.gameTickLoop
  -> GamePlay.onGameTick dt
      -> updateBattle dt for each battle
      -> respawn countdown
  -> dispatch responses
  -> saveAllPlayerSaves when there are responses
```

## 并发模型

当前使用两个主要线程：

- WebSocket server 线程：处理连接和玩家动作。
- game tick 线程：约每秒 tick 一次。

共享状态通过 `MVar GameState` 和 `MVar ServerMap` 串行修改，避免直接并发写状态。
