# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **Wuxia-themed MUD (Multi-User Dungeon)** game written in Haskell with a WebSocket-based architecture. The project is in active development (WIP) and aims to deliver a web UI-based multiplayer text adventure game with martial arts combat, skills, and character progression.

**Tech Stack:**
- Haskell (GHC 9.10.3, LTS 24.19)
- Stack build tool
- WebSockets for real-time client-server communication
- YAML-based game content configuration
- Lens for functional state management
- MTL monad transformers for effect composition

## Build Commands

```bash
# Build the project
stack build

# Run the server (listens on 127.0.0.1:9160)
stack exec mud-hs-exe

# Run tests
stack test

# Clean build artifacts
stack clean

# GHC REPL with project loaded
stack ghci
```

**Important:** The project uses `allow-newer: true` in stack.yaml to handle version mismatches between dependencies. This is required for GHC 9.10.3 compatibility.

## Architecture

### Core Architecture Pattern

The codebase follows a **functional, monad-transformer-based architecture** with clear separation between:

1. **Game State Layer** (`GameState.hs`) - Global state management
2. **Combat Layer** (`Game/Combat.hs`) - Battle-specific state
3. **World Layer** (`Game/World.hs`) - Static game data (maps, NPCs, skills)
4. **Server Layer** (`Server.hs`) - Network communication
5. **Gameplay Layer** (`GamePlay.hs`) - Game logic orchestration

### Monad Transformer Stack

**GameStateT** (main game monad):
```haskell
WriterT [PlayerResp] (StateT GameState (ExceptT GameException IO))
```
- Writer: Accumulates responses to send to players
- State: Manages mutable game state (players, battles, respawn timers)
- Except: Handles game exceptions
- IO: For side effects

**Combat** (battle subsystem monad):
```haskell
RWST World [PlayerResp] Battle (ExceptT CombatException (Rand StdGen))
```
- Reader: Accesses immutable world data
- Writer: Accumulates combat messages
- State: Manages battle state (AP, Qi, cooldowns, effects)
- Except: Handles combat errors
- Rand: Random number generation for combat

### Threading Model

The application uses **two main threads**:

1. **Game Tick Thread** - Runs every 1 second, handles:
   - Battle state updates (AP accumulation, Qi regeneration, effect durations)
   - NPC respawn timers
   - Time-based game mechanics

2. **WebSocket Server Thread** - Handles:
   - Player connections/disconnections
   - Incoming player actions (JSON messages)
   - Outgoing game responses
   - Keep-alive (ping/pong every 30s)

Both threads access shared state through MVars for thread-safe concurrent access.

### Data Flow

**Player Action → Response:**
```
Client WebSocket → Server.hs → GamePlay.hs → GameState monad
                                            ↓
                                   State update + Writer responses
                                            ↓
                                   Server.hs → Client WebSocket
```

**Battle Tick:**
```
Game Tick Loop → GamePlay.onGameTick → updateBattle → Combat monad
                                                     ↓
                                            State updates + messages
                                                     ↓
                                            GameState updated → Clients
```

## Key Subsystems

### Skill System (Recently Implemented)

The skill system uses a **requirement-based auto-validation** model:

- **Requirements:** AP threshold + Qi cost + cooldown + status effects
- **Resources:**
  - AP (Action Points): Accumulates based on Agility, threshold for actions
  - Qi (like MP): Consumed by skills, regenerates over time
- **Effects:** DoT (Damage over Time), HoT (Heal over Time), Buff, DeBuff
- **Targeting:** Single, Self, All

**Player-initiated casting:**
```
Player → {"perform": "skill_id"} → GamePlay.playerPerformSkill
      → Validate requirements → Combat.castSkill → Apply effects immediately
```

Skills are configured in YAML (`resources/scripts/skills/*.yaml`) with:
```yaml
id: power_strike
cost: 30          # Qi cost
ap_req: 60        # AP requirement
req_status: []    # Required buffs/debuffs
damage: 35
effect:
  self: []        # Effects applied to caster
  target: []      # Effects applied to target
```

### YAML Configuration System

Game content is **data-driven** through YAML files:

- `resources/scripts/maps/` - Map definitions with rooms and exits
- `resources/scripts/characters/` - NPCs with stats, dialogue, skills
- `resources/scripts/skills/` - Martial arts, moves, and skills
- `resources/scripts/effects/` - Status effect definitions
- `resources/scripts/default_player.yaml` - Player template

**Loading pattern:**
```haskell
-- Entity.hs defines Configurable typeclass
class Configurable a where
  loadConfig :: FilePath -> IO (Either Text a)
  loadConfigBatch :: FilePath -> IO (Either Text [a])

-- World.hs uses generic loading
loadMartialArts :: FilePath -> IO (Either WorldException (M.Map ArtId MartialArt))
```

Add new content by creating YAML files following existing patterns. No code changes required for new skills, NPCs, or maps.

### Combat System

Combat is **turn-based with AP accumulation**:

1. Each character accumulates AP at rate = `agility * dt`
2. When AP ≥ 100 (or skill AP requirement met):
   - Character can attack (basic move) or cast skill
   - AP resets or is consumed
3. Skills check: Qi cost, cooldown, status prerequisites
4. Damage/healing applied immediately
5. Effects track duration, auto-expire

**Battle lifecycle:**
```
Player attacks NPC → newBattle created → Player status = InBattle
                                       ↓
                           Game tick calls updateBattle
                                       ↓
                       Both sides accumulate AP, regenerate Qi
                                       ↓
                       Skills cast when requirements met
                                       ↓
                       Battle ends when HP ≤ 0 → Settlement
```

## Code Conventions

### Lens Usage
The codebase heavily uses lens for state manipulation:
```haskell
-- Reading nested state
player <- getsPlayer pid
let hp = player ^. playerCharacter . charHP

-- Updating nested state
players . ix pid . playerCharacter . charHP -= damage
```

### Error Handling
- Use `GameException` for game logic errors
- Use `CombatException` for battle errors
- Use `WorldException` for data loading errors
- Always use `throwError` in monad transformers, never `error`

### Naming Patterns
- `_fieldName` - Record fields (lens will generate `fieldName` accessor)
- `getsPlayer`, `getsBattle` - State getters in GameState monad
- `liftWorld` - Lift World operations into GameState
- `new*` - Constructor functions (e.g., `newBattle`, `newPlayer`)

### Aeson/JSON Compatibility (GHC 9.10.3+)

**Important:** Use `Data.Aeson.KeyMap` instead of `HashMap` for JSON objects:

```haskell
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

-- Pattern for parsing nested JSON:
parseEffectItem (Object o) =
  let effId = fromJSON <$> KM.lookup (Key.fromString "id") o
```

Avoid `Data.HashMap.Strict` for Aeson `Object` types - this will cause type errors.

### Common Pitfalls

1. **Ambiguous `show` and `ap`**: Always qualify with `Prelude.show` when `Data.Text` is imported
2. **`unless` location**: Import from `Control.Monad`, not `Control.Monad.Except`
3. **Battle state isolation**: Combat monad can't directly modify GameState - return updated Battle
4. **MVar deadlocks**: Always use `modifyMVar_` pattern, never nest MVar operations

## Project Status & Milestones

**Current Status:** Core gameplay loop functional, skills system implemented

**Completed:**
- ✅ WebSocket server with multi-client support
- ✅ Movement system (8-directional)
- ✅ Turn-based combat with AP system
- ✅ Skill system with Qi resource, requirements, and effects
- ✅ NPC dialogue and respawn
- ✅ YAML-based content configuration

**In Progress / Missing:**
- ⏳ Client (web UI) - server is ready but no client exists
- ⏳ Item system (data structures exist, not integrated)
- ⏳ Quest system (basic types defined, not implemented)
- ⏳ Character progression (no leveling or skill learning)
- ⏳ Database persistence (Database.hs is empty stub)
- ⏳ Battle rewards and economy

When adding features, follow the existing monad transformer patterns and use YAML configuration for content.
