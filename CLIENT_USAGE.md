# Minimal Web Client - Quick Start Guide

## Running the Client

### Step 1: Start the Server
```bash
stack run
```
The server should start on `ws://127.0.0.1:9160`

### Step 2: Open the Client
Simply open `client.html` in your web browser:
```bash
# On Linux
xdg-open client.html

# On Mac
open client.html

# On Windows
start client.html

# Or just double-click the file
```

### Step 3: Login
1. Enter any username
2. Click "Enter the World"
3. You should see the connection status turn green and receive initial messages

## Using the Client

### Interface Overview

**Main Message Area (Left)**: Displays all game messages with color coding:
- Blue: Movement messages
- Red: Combat messages
- Green: Dialogue and chat
- Gray: System messages

**Sidebar (Right)**:
- **Current Location**: Shows room name and description
- **Movement**: 8-directional buttons (only active directions are clickable)
- **Characters in Room**: NPCs with Talk/Attack buttons

**Command Input (Bottom)**: Type commands and press Enter

### Available Commands

| Command | Example | Description |
|---------|---------|-------------|
| `go <direction>` | `go N` | Move in direction (N, S, E, W, NE, NW, SE, SW) |
| `talk <name>` | `talk Momo the Tester` | Talk to NPC |
| `attack <name>` | `attack Momo the Tester` | Attack NPC |
| `look` | `look` | Refresh room view |
| `say <message>` | `say Hello!` | Chat (not implemented server-side yet) |

**Combat Commands (during battle):**

| Command | Example | Description |
|---------|---------|-------------|
| `perform <skill_id>` | `perform test_skill_heavy_strike` | Use a skill |

**Available Skills:**
- `test_skill_heavy_strike` - Heavy damage (30), 5s cooldown, costs 50 AP
- `test_skill_healing_palm` - Heal self (25 HP), 10s cooldown, costs 30 AP
- `test_skill_quick_jab` - Quick damage (15), 2s cooldown, costs 20 AP

**Shortcuts**:
- **Direction buttons**: Click to move instantly
- **NPC buttons**: Click Talk or Attack for quick actions
- **Arrow Up/Down**: Navigate command history
- **Enter**: Send command

### Testing the Game

1. **Movement Test**:
   ```
   go N
   go E
   go S
   go W
   ```

2. **NPC Interaction**:
   ```
   talk Momo the Tester
   ```
   You should see a random dialogue response.

3. **Combat Test**:
   ```
   attack Momo the Tester
   ```
   Combat should initiate and you'll see combat messages as the battle progresses automatically.

### Troubleshooting

**"Connection error"**:
- Make sure the Haskell server is running
- Check that it's listening on port 9160
- Try refreshing the page

**"Not connected to server"**:
- The WebSocket connection was lost
- Refresh the page to reconnect

**No response to commands**:
- Check the browser console (F12) for errors
- Verify command syntax
- Check server logs

**NPCs not showing**:
- Type `look` to refresh the room view
- Make sure you're in a room with NPCs

### Browser Console

Open browser developer tools (F12) to see:
- Raw server messages
- WebSocket connection status
- Any JavaScript errors

All received messages are logged with: `Received: <message>`

## Features Implemented

✓ WebSocket connection with auto-detect
✓ Login screen
✓ Real-time message display
✓ Command parsing (go, talk, attack, etc.)
✓ Room view with NPCs and exits
✓ Direction pad with disabled states
✓ NPC quick-action buttons
✓ Command history (arrow keys)
✓ Color-coded messages
✓ Connection status indicator
✓ Responsive scrolling message area

## Known Limitations

- Combat is auto-resolved (no player skill selection yet)
- No character stats display (HP, AP, etc.)
- No inventory UI
- No reconnection on disconnect
- Single-user session only (no persistence)

## Next Steps for Testing

1. Test basic movement around the map
2. Try talking to NPCs
3. Initiate combat with an NPC
4. Observe combat messages
5. Wait for NPC respawn after defeating them
6. Try moving to different rooms

Enjoy testing the game!
