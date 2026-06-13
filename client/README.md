# Wuxia MUD Web Client

Svelte + TypeScript client for the Haskell WebSocket MUD server.

The UI is intentionally simple: dark background, crisp frame lines, dense text panels, and no image-based scene or sprite dependencies.

## Run

Start the game server first:

```bash
stack exec mud-hs-exe
```

Then run the client dev server:

```bash
cd client
npm install
npm run dev
```

Open:

```text
http://127.0.0.1:8080/
```

## Test Entry

For local story-flow testing:

```bash
MUD_DEV_MODE=1 stack exec mud-hs-exe
```

Then open:

```text
http://127.0.0.1:8080/?test=1
```

The client auto-connects as `tester` and requests a fresh save reset. Use `?test=1&user=name` for a fixed alternate user, or `?test=1&reset=0` to auto-login without resetting.

## Scripts

```bash
npm run dev
npm run check
npm run build
```

## Protocol

The client connects to `ws://127.0.0.1:9160`.

Login:

```json
{"tag":"Login","username":"player","password":""}
```

Actions:

```json
{"tag":"NetPlayerAction","contents":{"go":"North"}}
{"tag":"NetPlayerAction","contents":{"talk":"npc_id"}}
{"tag":"NetPlayerAction","contents":{"attack":"npc_id"}}
{"tag":"NetPlayerAction","contents":{"perform":"active_skill_id"}}
{"tag":"NetPlayerAction","contents":{"train":"art_id"}}
{"tag":"NetPlayerAction","contents":{"use":"item_id"}}
```

Core server messages handled by the client:

- `ViewMsg`
- `MoveMsg`
- `PlayerStatsMsg`
- `BattleStateMsg`
- `CombatNormalMsg`
- `ActiveSkillMsg`
- `CombatSettlementMsg`
- `QuestLogMsg`
- `InventoryMsg`
- `ArtsMsg`
- `DialogueMsg`
- `StoryMsg`
- `SystemMsg`
- `ErrorMsg`
