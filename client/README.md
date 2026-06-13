# Wuxia MUD Web Client

A native web client for the Wuxia MUD game, styled as a dark Wuxia scene UI rather than a dashboard.

## Features

- **WebSocket Connection** - Real-time communication with game server
- **Scene Layout** - Player state, local room scene, message history, and martial arts panels
- **Resource Bars** - Visual health, inner power, and action bars using localized UI labels
- **Battle Scene** - Dedicated silhouette combat view with player/enemy resources and queued action playback
- **Martial Arts Panel** - Learned martial arts outside combat and active move cards during combat
- **Room Panel** - Clickable relative-position map nodes and character names
- **Combat Log** - Collapsible log of combat events
- **Message Display** - Color-coded messages with timestamps
- **NPC Popup** - Talk and attack actions for the selected character
- **Dev Test Entry** - Optional URL-based auto-login/reset for repeated local flow testing

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
|  Health       |  | Message history                            |  |
|  Inner Power  |  +--------------------------------------------+  |
|  Action       |  +--------------------------------------------+  |
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

**Active moves (in combat):**
- Click active move cards to use unlocked moves from prepared martial arts.
- Active move cards show inner power cost, action requirement, cooldown, and status requirements.
- Disabled cards are on cooldown or missing action/inner-power/status requirements.
- Combat messages and silhouette actions are queued so simultaneous server actions play in order.

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

## Martial Arts Data

The client does not hard-code available martial arts or move names. It renders `ArtsMsg` outside combat and `BattleStateMsg.battleSnapshotActiveSkills` during combat.

Current content examples include:

- `无名试刀`: starter fist art with `断雨一击` and `回灯掌`.
- `冷雨短打`: fist art with staged active move unlocks.
- `听雨残谱`: sword art learned by using the manual item from the cold-rain chapter.

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
{"tag": "NetPlayerAction", "contents": {"perform": "lamp_cut"}}
{"tag": "NetPlayerAction", "contents": {"train": "cold_rain_secret"}}
{"tag": "NetPlayerAction", "contents": {"use": "cold_rain_manual"}}
```

**Server Responses:**
```json
{"tag": "MoveMsg", "contents": "Room Name"}
{"tag": "ViewMsg", "contents": ["Room", "Desc", [{"id":"npc","name":"NPC","desc":"...","actions":["talk"]}], [{"direction":"North","mapId":"next_map","roomId":"next","roomName":"Next","position":[0,1]}]]}
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

**Active moves not working?**
- Active moves only work during combat
- Check if you have enough inner power and action
- Some active moves require a status effect, such as `伞骨八刺` requiring `听雨势`

**Buttons not responding?**
- Make sure you're connected first
- For normal entry, check that username was entered and "Connect" was clicked
- For test entry, start the server with `MUD_DEV_MODE=1` and open `?test=1`
