# Wuxia MUD Web Client

A web-based client for the Wuxia MUD game featuring a modern dark theme UI with real-time combat tracking.

## Features

- **WebSocket Connection** - Real-time communication with game server
- **Three-Panel Layout** - Player status, game display, and room info
- **Resource Bars** - Visual HP, Qi, and AP bars with color gradients
- **Battle Panel** - Animated combat indicator with player vs enemy display
- **Skills Panel** - Clickable skill cards with cost, AP requirements, and cooldown tracking
- **Room Panel** - Interactive character list and exit buttons
- **Combat Log** - Collapsible log of combat events
- **Message Display** - Color-coded messages with timestamps
- **Command Input** - Text input with shortcut support (n/s/e/w for directions)

## How to Use

### 1. Start the Server

First, make sure the game server is running:

```bash
cd /home/nomofu/code/hs-wuxia-mud
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
3. Use the action buttons or type commands manually
4. Explore the world!

## UI Layout

```
+------------------------------------------------------------------+
|  Header: Wuxia MUD                           [Connection Status] |
+---------------+----------------------------+---------------------+
|  PLAYER       |  GAME DISPLAY              |  LOCATION           |
|  STATUS       |  +----------------------+  |                     |
|               |  | Battle Panel         |  |  Room Name          |
|  Name: xxx    |  | [IN COMBAT]          |  |  Room description   |
|  Location:    |  | Player VS Enemy      |  |                     |
|  Status:      |  +----------------------+  |  CHARACTERS HERE    |
|               |                            |  - Character 1      |
|  HP [====   ] |  +----------------------+  |  - Character 2      |
|  Qi [======  ]|  | Message Area         |  |                     |
|  AP [==      ]|  | (scrollable)         |  |  EXITS              |
|               |  +----------------------+  |  - North            |
|  ACTIVE       |                            |  - South            |
|  EFFECTS      |  [Command Input] [Send]    |                     |
|  - None       |                            |                     |
|               |  +----------------------+  |                     |
|  LOGIN        |  | Action Buttons:      |  |                     |
|  [Username]   |  | Movement | Interact  |  |                     |
|  [Connect]    |  | Skills Panel         |  |                     |
+---------------+----------------------------+---------------------+
|  Combat Log (collapsible)                                        |
+------------------------------------------------------------------+
```

## Commands

### Using Buttons

**Movement Grid:**
- Click directional buttons (N, S, E, W, NE, NW, SE, SW)
- Or click "Look" to view current location

**Interactions:**
- Talk: Click, enter character ID to start dialogue
- Attack: Click, enter character ID to start combat
- Say: Click, enter message to say

**Skills (In Combat):**
- Click skill cards to use martial arts
- Skills show Qi cost, AP requirement, and effects
- Grayed out skills are on cooldown or missing requirements

**Room Panel:**
- Click character names for quick interact menu
- Click exits to move in that direction

### Manual Commands

Type commands in the input field:

```
# Movement (multiple formats)
go north              # Move north
north                 # Move north
n                     # Move north

# Combat
attack char_in_test   # Attack an NPC
perform power_strike  # Use a skill

# Interaction
talk char_in_test     # Talk to an NPC
say Hello everyone!   # Say something

# View
look                  # View current room
view                  # View current room
l                     # View current room
```

## Message Types

Messages are color-coded:

| Type | Color | Description |
|------|-------|-------------|
| System | Gray | General messages |
| Move | Blue | Movement messages |
| Combat | Red | Combat actions and damage |
| Skill | Purple | Skill usage |
| Dialogue | Green | NPC conversations |
| Say | Yellow | Player chat |
| Error | Red bg | Error messages |

## Skills

Available martial arts skills:

| Skill | Qi Cost | AP Req | Effect |
|-------|---------|--------|--------|
| Power Strike | 30 | 60 | 35 damage |
| Healing Palm | 50 | 40 | 40 heal (self) |
| Iron Body | 40 | 50 | DEF +5 buff |
| Dragon Stance | 40 | 50 | Enables Dragon Burst |
| Dragon Burst | 80 | 100 | 120 damage (requires Dragon Stance) |

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
{"tag": "CombatNormalMsg", "contents": ["Attacker", "Defender", "msg", 35]}
{"tag": "PlayerStatsMsg", "contents": [100, 100, 80, 100, 45, "In Battle"]}
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

**Skills not working?**
- Skills only work during combat
- Check if you have enough Qi and AP
- Some skills require specific buffs (e.g., Dragon Burst needs Dragon Stance)

**Buttons not responding?**
- Make sure you're connected first
- Check that username was entered and "Connect" was clicked
