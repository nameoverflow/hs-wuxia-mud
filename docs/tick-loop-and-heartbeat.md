# Tick 循环、心跳与战斗推进

本文记录 server tick 的当前实现、从传统武侠 MUD 心跳机制中借鉴的设计，以及后续战斗推进规划。

## 当前实现

server 在 [src/Server.hs](../src/Server.hs) 中启动独立 tick 线程：

```text
gameTickLoop
  -> sleep 1s
  -> compute real dt
  -> GamePlay.onGameTick dt
  -> dispatch responses
  -> persist dirty players only
```

`dt` 使用真实时间差，而不是固定写死 1 秒。这能在 tick 被阻塞或机器负载较高时保持资源恢复、AP 累积和 cooldown 递减的大致一致性。

游戏逻辑入口在 [src/GamePlay.hs](../src/GamePlay.hs)：

```text
onGameTick dt
  -> tickBattles dt
  -> tickRespawns dt
```

当前 phase：

- `tickBattles`：推进所有进行中的战斗。
- `tickRespawns`：推进 NPC 复活计时。

## 持久化策略

tick 中有大量只服务 UI 的临时响应，例如 AP、Qi、cooldown 和战斗快照。它们不应该每秒触发全员存档。

当前 server 区分两种保存策略：

- 普通玩家动作：沿用 `SaveOnAnyResponse`，有响应或 dirty player 时保存。
- game tick：使用 `SaveDirtyPlayers`，只保存被标记为 dirty 的玩家。

`GameState` 中维护运行时字段：

```text
dirtyPlayers :: Set PlayerId
```

当前会在战斗结算后标记玩家 dirty，因为结算可能改变：

- 玩家战斗状态。
- 玩家持久化剧情状态。
- 背包、金钱、武功等奖励。
- 已完成剧情任务。

当前玩家 HP/Qi/MaxQi 已进入 `PlayerSave`，战斗结算和打坐后的长期资源变化都会跟随玩家存档保存。

## 从传统 MUD 借鉴的心跳原则

传统 LPMUD 里每个 living object 可以有自己的 `heart_beat()`。这种模型很灵活，但在当前 Haskell 项目中不适合照搬，因为它会削弱集中状态和测试能力。

可借鉴的是心跳职责拆分：

```text
busy / continue action
auto flee
combat attack
NPC scan / AI
condition update
heal / resource recovery
autosave
idle / hunger / world rules
```

当前项目应继续保留集中 tick，但按系统拆 phase：

```text
tickBattles
tickConditions
tickRespawns
tickNpcAi
tickWorldJobs
tickPersistence
```

拆分原则：

- 高频 phase 只处理必须每秒推进的逻辑。
- 慢速 phase 使用独立计时器，不要每秒扫全量对象。
- 只 tick 活跃对象，例如战斗中的 NPC、有 condition 的角色、有玩家在附近的区域。
- UI 快照和持久化保存分离。

## 战斗 tick 当前流程

战斗内部在 [src/Game/Combat.hs](../src/Game/Combat.hs)：

```text
flushBattleTick dt
  -> active skill cooldown -= dt
  -> apply DoT/HoT
  -> expire effects
  -> if dead, finish
  -> regenerate Qi
  -> accumulate AP
  -> player auto attack if AP >= 100
  -> enemy auto attack if AP >= 100
  -> return whether battle is over
```

战斗结算在 [src/GamePlay.hs](../src/GamePlay.hs)：

```text
updateBattle
  -> run Combat.flushBattleTick
  -> send battle snapshot
  -> if over, battleSettlement
  -> else write updated Battle back to GameState
```

结算会：

- 移除 battle。
- 玩家回到 `PlayerNormal`。
- 回写玩家 HP/Qi 到运行时角色。
- 若玩家获胜，标记 NPC 死亡并设置 respawn。
- 触发 kill 剧情。
- 发送结算和玩家状态。
- 标记玩家 dirty，等待 tick 保存策略写盘。

## 普通攻击流水线

普通攻击已经不再是固定伤害。当前流程：

```text
selectAttackMove
rollHit
rollDodge
rollParry
computeDamage
applyCombatHooks
applyDamageAndEffects
emitCombatEvents
```

当前已接入：

- 命中。
- 闪避。
- 招架。
- 基于 `str`、`agi`、`vit`、武功等级和招式基础伤害的伤害计算。

仍预留后续接入：

- 护甲。
- 武器。
- 暴击。
- 内功反震。
- 技能命中 hook。
- 状态效果 hook。

## Busy 规划

传统 MUD 的 `busy` 是很重要的节奏控制机制。它可以表示出招后摇、被控制、疗伤打坐中、读书中等状态。

建议后续加入：

```text
busyRemaining :: Double
busyReason    :: Text
```

战斗中的效果：

- busy 时不能主动 `perform`。
- busy 时普通攻击暂停或 AP 增长降低。
- busy 时闪避/招架降低。
- 强力主动招式可配置 `busy_self`。
- 控制类招式可配置 `busy_target`。

非战斗中的效果：

- 读书、打坐、吐纳、采集等动作可以进入 busy。
- 移动、攻击、交谈等命令可根据 busy 类型决定是否允许。

## 当前已落地

- `onGameTick` 已拆为 `tickBattles` 和 `tickRespawns`。
- server tick 已使用 `SaveDirtyPlayers`，不再因为每秒战斗快照触发全员存档。
- `GameState` 已增加 `dirtyPlayers`。
- 战斗结算后会 `markPlayerDirty`。
- 普通攻击已拆为接近传统 MUD 的命中/闪避/招架/伤害流水线。
- `PlayerSave` 已增加 `version`、潜能、实战经验、HP/Qi/MaxQi 和 `enabled`。
- 战斗胜利已接入实战经验和潜能奖励。

## 下一步

优先级建议：

1. 增加 `tickConditions`，把战斗内外 condition 统一。
2. 增加 busy 机制，让 learn/practice/study/research/meditate 走耗时动作。
3. 为普通攻击流水线接入装备、护甲、暴击和技能 hook。
4. 引入慢速 tick phase，处理 NPC AI、动态任务和世界事件。
5. 在 `PlayerSave.version` 基础上补显式迁移函数。
