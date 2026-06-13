# Wuxia MUD Web Client

A web-based client for the Wuxia MUD game featuring a modern dark theme UI with real-time combat tracking.

## Features

- **WebSocket Connection** - Real-time communication with game server
- **Three-Panel Layout** - Player status, game display, and room info
- **Resource Bars** - Visual HP, Qi, and AP bars with color gradients
- **Battle Panel** - Animated combat indicator with player vs enemy display
- **Martial Arts Panel** - Learned martial arts outside combat and active skill cards during combat
- **Room Panel** - Clickable map nodes and character names
- **Combat Log** - Collapsible log of combat events
- **Message Display** - Color-coded messages with timestamps
- **NPC Popup** - Talk and attack actions for the selected character

## How to Use

### 1. Start the Server

First, make sure the game server is running:

```bash
cd /Users/nomofu/code/hs-wuxia-mud
stack exec mud-hs-exe
```

The server should start on `127.0.0.1:9160`.

### 2. Open the Client

Simply open `index.html` in a web browser:

```bash
# Option 1: Direct file open
open client/index.html  # macOS
xdg-open client/index.html  # Linux
start client/index.html  # Windows

# Option 2: Use a local HTTP server (recommended)
cd client
python3 -m http.server 8080
# Then navigate to http://localhost:8080
```

### 3. Connect and Play

1. Enter your username in the login panel
2. Click "Connect"
3. Click map nodes and character names to interact
4. Explore the world!

### Test Entry

For local flow testing, start the server with dev mode:

```bash
MUD_DEV_MODE=1 stack exec mud-hs-exe
```

Then open:

```text
http://localhost:8080/?test=1
```

The test entry auto-connects as `tester` and requests a fresh save reset. Use `?test=1&user=name` for another fixed test user, or `?test=1&reset=0` to auto-login without resetting the save.

## UI Layout

```
+------------------------------------------------------------------+
|  Header: Wuxia MUD                           [Connection Status] |
+---------------+--------------------------------------------------+
|  PLAYER       |  GAME DISPLAY                                    |
|  STATUS       |  +--------------------------------------------+  |
|               |  | Local map / room scene                     |  |
|  Name: xxx    |  | Click room nodes to move                   |  |
|  Location:    |  | Click character names to open popup        |  |
|  Status:      |  +--------------------------------------------+  |
|               |  +--------------------------------------------+  |
|  HP [====   ] |  | Message history                            |  |
|  Qi [======  ]|  +--------------------------------------------+  |
|  AP [==      ]|  +--------------------------------------------+  |
|               |  | Martial arts / active skills               |  |
|  LOGIN        |  +--------------------------------------------+  |
|  [Username]   |                                                  |
|  [Connect]    |                                                  |
+---------------+--------------------------------------------------+
|  Combat Log (collapsible)                                        |
+------------------------------------------------------------------+
```

## UI Interactions

**Movement:**
- Click a room node in the local map.
- Exit names are a fallback for the same movement action.

**NPCs and story:**
- Click a character name to open the interaction popup.
- Clicking Talk closes the popup and appends dialogue/story text to message history.
- Attack is started from the same popup when the character exposes that action.

**Active Skills (In Combat):**
- Click active skill cards to use prepared martial arts
- Active skills show Qi cost, AP requirement, and effects
- Grayed out active skills are on cooldown or missing requirements

## Message Types

Messages are color-coded:

| Type | Color | Description |
|------|-------|-------------|
| System | Gray | General messages |
| Move | Blue | Movement messages |
| Combat | Red | Combat actions and damage |
| Active Skill | Purple | Active skill usage |
| Dialogue | Green | NPC conversations |
| Say | Yellow | Player chat |
| Error | Red bg | Error messages |

## Active Skills

Available active skills:

| Active Skill | Qi Cost | AP Req | Effect |
|-------|---------|--------|--------|
| 断雨一击 | 30 | 60 | 35 damage |
| 回灯掌 | 50 | 40 | 40 heal (self) |
| 铁衣 | 40 | 50 | DEF +5 buff |
| 听雨势 | 40 | 50 | Enables 伞骨八刺 |
| 伞骨八刺 | 80 | 100 | 120 damage (requires 听雨势) |

## Technical Details

### WebSocket Protocol

The client connects to `ws://127.0.0.1:9160` and uses JSON messages:

**Login:**
```json
{"tag": "Login", "username": "player", "password": ""}
```

**Actions:**
```json
{"tag": "NetPlayerAction", "contents": {"go": "North"}}
{"tag": "NetPlayerAction", "contents": {"attack": "char_id"}}
{"tag": "NetPlayerAction", "contents": {"perform": "power_strike"}}
```

**Server Responses:**
```json
{"tag": "MoveMsg", "contents": "Room Name"}
{"tag": "ViewMsg", "contents": ["Room", "Desc", ["char1"], ["North"]]}
{"tag": "CombatNormalMsg", "contents": ["Attacker", "Defender", {"kind": "script", "text": "msg"}, 35]}
{"tag": "PlayerStatsMsg", "contents": [100, 100, 80, 100, 45, "in_battle"]}
```

### Browser Compatibility

- Modern browsers with WebSocket support
- Tested on Chrome, Firefox, Safari, Edge
- Responsive design (works on mobile and tablet)

## Troubleshooting

**Can't connect?**
- Ensure server is running on 127.0.0.1:9160
- Check browser console for WebSocket errors
- Try using `python3 -m http.server` instead of file://

**Active skills not working?**
- Active skills only work during combat
- Check if you have enough Qi and AP
- Some active skills require specific buffs (e.g., 伞骨八刺 needs 听雨势)

**Buttons not responding?**
- Make sure you're connected first
- Check that username was entered and "Connect" was clicked
