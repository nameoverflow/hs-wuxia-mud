# Web UI Client Design & Implementation Plan
## Haskell Wuxia MUD Game

---

## 1. Design Overview

### 1.1 Architecture Choice

**Single Page Application (SPA)** with vanilla JavaScript/TypeScript
- No heavy framework dependencies (React/Vue) for simplicity
- Direct WebSocket communication with Haskell server
- Modular component-based architecture
- Static file serving (can be served by simple HTTP server or integrated into Haskell server)

### 1.2 Technology Stack

**Core Technologies:**
- **HTML5** - Semantic markup
- **CSS3** - Styling with CSS Grid/Flexbox
- **TypeScript** - Type-safe client code (compiles to JavaScript)
- **WebSocket API** - Real-time bidirectional communication
- **LocalStorage** - Client-side username persistence

**Optional Enhancements:**
- **Vite** - Development server with hot reload
- **Tailwind CSS** or custom CSS - Consistent styling
- **Marked.js** - If we want to support markdown in descriptions

### 1.3 UI Layout

```
┌─────────────────────────────────────────────────────────┐
│  HEADER: Wuxia MUD - Character: [Username] HP: [100/100]│
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌───────────────────────┐  ┌──────────────────────┐  │
│  │                       │  │  CHARACTER STATUS     │  │
│  │                       │  │  ─────────────────    │  │
│  │   GAME OUTPUT         │  │  HP: ████████░░ 80/100│  │
│  │   (Message Feed)      │  │  Strength: 15         │  │
│  │                       │  │  Agility: 12          │  │
│  │   - Auto-scrolling    │  │  Vitality: 10         │  │
│  │   - Color-coded msgs  │  │                      │  │
│  │   - Timestamps        │  │  STATUS: Normal       │  │
│  │                       │  │                      │  │
│  │                       │  │  POSITION:           │  │
│  │                       │  │  Test Map (3, 3)     │  │
│  │                       │  └──────────────────────┘  │
│  │                       │  ┌──────────────────────┐  │
│  │                       │  │  QUICK ACTIONS       │  │
│  │                       │  │  ─────────────────    │  │
│  │                       │  │  [N] [NE] [E] [SE]   │  │
│  │                       │  │  [W] [  ] [  ] [S]   │  │
│  │                       │  │  [NW][  ] [  ] [SW]  │  │
│  │                       │  │                      │  │
│  │                       │  │  IN ROOM:            │  │
│  │                       │  │  • Momo the Tester   │  │
│  │                       │  │    [Talk] [Attack]   │  │
│  └───────────────────────┘  └──────────────────────┘  │
│                                                         │
├─────────────────────────────────────────────────────────┤
│  COMBAT PANEL (shown during battle)                     │
│  ┌─────────────────────────────────────────────────┐   │
│  │  BATTLE: You vs Momo the Tester                 │   │
│  │  Your HP: ████████░░ 80/100  Enemy: ██████████ │   │
│  │  Your AP: ████░░░░░░ 40/100                     │   │
│  │                                                  │   │
│  │  [Basic Attack] [Skill 1] [Skill 2] [Use Item]  │   │
│  └─────────────────────────────────────────────────┘   │
├─────────────────────────────────────────────────────────┤
│  INPUT: > [command text field]              [Send]     │
└─────────────────────────────────────────────────────────┘
```

---

## 2. Technical Specifications

### 2.1 WebSocket Protocol

**Connection Flow:**
1. User enters username (and password placeholder)
2. Client connects to `ws://127.0.0.1:9160`
3. Client sends Login event: `{"username": "player1", "password": ""}`
4. Server responds with initial ViewMsg
5. Client enters game loop, sending NetPlayerAction events

**Message Types (Client → Server):**

```typescript
type NetEvent =
  | { username: string; password: string }  // Login
  | { NetPlayerAction: PlayerAction }       // Game action
  | "Disconnect"

type PlayerAction =
  | { go: Direction }           // Movement
  | { talk: string }            // Talk to NPC (CharId)
  | { attack: string }          // Attack NPC (CharId)
  | { perform: string }         // Use skill (SkillId)
  | { use: string }             // Use item
  | { say: string }             // Chat message
  | { other: string }           // Custom command

type Direction = "N" | "S" | "E" | "W" | "NE" | "NW" | "SE" | "SW"
```

**Message Types (Server → Client):**

```typescript
type ActionResp =
  | { MoveMsg: string }                                    // Room name
  | { ViewMsg: [string, string, string[], Direction[]] }   // [name, desc, chars, exits]
  | { AttackMsg: [string, string] }                        // [attacker, defender]
  | { SkillMsg: [string, string, string] }                 // [attacker, defender, skill]
  | { CombatNormalMsg: [string, string, string, number] }  // [attacker, defender, desc, damage]
  | { CombatSettlementMsg: [string, string, boolean] }     // [attacker, defender, win]
  | { UseItemMsg: [string, string] }                       // [user, item desc]
  | { SayMsg: [string, string] }                           // [speaker, message]
  | { DialogueMsg: [string, string] }                      // [char name, dialogue]
```

### 2.2 Client State Management

```typescript
interface GameState {
  // Connection
  connected: boolean;
  websocket: WebSocket | null;
  username: string;

  // Player state
  playerStatus: "Normal" | "InBattle" | "Dead" | "Banned";
  hp: number;
  maxHp: number;
  strength: number;
  agility: number;
  vitality: number;

  // Current location
  currentRoom: string;
  roomDescription: string;
  mapPosition: [number, number] | null;

  // Environment
  npcsInRoom: Array<{id: string, name: string}>;
  playersInRoom: string[];
  availableExits: Direction[];

  // Combat state
  inBattle: boolean;
  battleEnemy: string | null;
  enemyHp: number;
  enemyMaxHp: number;
  playerAp: number;
  availableSkills: Array<{id: string, name: string, cooldown: number}>;

  // UI state
  messages: Array<{text: string, type: string, timestamp: Date}>;
  commandHistory: string[];
  historyIndex: number;
}
```

### 2.3 Component Breakdown

#### Core Components:

1. **App** - Root component, manages global state
2. **LoginScreen** - Initial login form
3. **GameContainer** - Main game interface container
4. **MessageFeed** - Scrolling message display
5. **StatusPanel** - Character stats display
6. **LocationPanel** - Current room info and NPCs
7. **DirectionPad** - Movement buttons
8. **CombatPanel** - Battle interface (conditional)
9. **CommandInput** - Text command entry
10. **ConnectionStatus** - WebSocket status indicator

---

## 3. Implementation Plan

### Phase 1: Core Infrastructure (Foundation)

**File Structure:**
```
client/
├── index.html              # Main HTML entry point
├── src/
│   ├── main.ts            # Application entry point
│   ├── websocket.ts       # WebSocket connection manager
│   ├── state.ts           # State management
│   ├── types.ts           # TypeScript type definitions
│   ├── message-parser.ts  # Parse server messages
│   └── components/
│       ├── app.ts         # Root component
│       ├── login.ts       # Login screen
│       └── game.ts        # Main game container
├── styles/
│   ├── main.css           # Global styles
│   ├── components.css     # Component-specific styles
│   └── themes.css         # Color schemes
├── package.json
├── tsconfig.json
└── vite.config.ts         # Build configuration
```

**Tasks:**
- [ ] Set up project structure with TypeScript and Vite
- [ ] Create type definitions matching Haskell server protocol
- [ ] Implement WebSocket connection manager with reconnection logic
- [ ] Build basic state management system (observer pattern)
- [ ] Create HTML skeleton with semantic structure
- [ ] Implement Login component

**Deliverable:** User can connect, login, and see connection status

---

### Phase 2: Core Game Interface

**Tasks:**
- [ ] Implement MessageFeed component with auto-scroll
- [ ] Create message parser for all ActionResp types
- [ ] Build StatusPanel showing character attributes
- [ ] Implement LocationPanel showing room info
- [ ] Create DirectionPad with 8-directional movement
- [ ] Implement CommandInput with command history (↑/↓ keys)
- [ ] Add message type color coding (movement, combat, dialogue, system)
- [ ] Display NPCs in room with action buttons

**Deliverable:** User can move around, see room descriptions, view NPCs

---

### Phase 3: Combat System

**Tasks:**
- [ ] Create CombatPanel component
- [ ] Implement combat state tracking (HP, AP bars)
- [ ] Add visual HP/AP bars with animations
- [ ] Create skill buttons (Basic Attack + skill slots)
- [ ] Parse and display combat messages
- [ ] Show battle start/end notifications
- [ ] Implement combat action cooldown indicators
- [ ] Add battle result screen

**Deliverable:** User can engage in combat, use attacks, see results

---

### Phase 4: Polish & UX

**Tasks:**
- [ ] Add CSS animations (HP changes, messages appearing)
- [ ] Implement sound effects (optional: attack, movement, notifications)
- [ ] Add command auto-completion
- [ ] Create help panel with available commands
- [ ] Add persistent settings (sound on/off, theme)
- [ ] Implement error handling and user-friendly error messages
- [ ] Add loading states and spinners
- [ ] Create responsive design for different screen sizes
- [ ] Add keyboard shortcuts (WASD for movement, numbers for skills)

**Deliverable:** Polished, user-friendly interface

---

### Phase 5: Advanced Features

**Tasks:**
- [ ] Implement mini-map visualization
- [ ] Add player inventory UI
- [ ] Create item usage interface
- [ ] Add skill/martial arts management screen
- [ ] Implement quest log UI (when backend ready)
- [ ] Add character sheet with detailed stats
- [ ] Create settings panel
- [ ] Add chat/social features UI

**Deliverable:** Full-featured client with all game systems

---

## 4. Detailed Implementation Specs

### 4.1 WebSocket Manager (`websocket.ts`)

```typescript
class WebSocketManager {
  private ws: WebSocket | null = null;
  private url: string;
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;
  private reconnectDelay = 2000;

  constructor(url: string) {
    this.url = url;
  }

  connect(username: string, password: string): Promise<void> {
    // Create WebSocket connection
    // Send Login event
    // Set up event handlers
    // Implement reconnection logic
  }

  send(action: PlayerAction): void {
    // Wrap in NetPlayerAction and send
  }

  onMessage(handler: (resp: ActionResp) => void): void {
    // Register message handler
  }

  disconnect(): void {
    // Clean disconnect
  }
}
```

### 4.2 State Manager (`state.ts`)

```typescript
class GameStateManager {
  private state: GameState;
  private listeners: Map<string, Function[]> = new Map();

  constructor() {
    this.state = this.getInitialState();
  }

  getState(): GameState {
    return this.state;
  }

  setState(partial: Partial<GameState>): void {
    // Merge state, notify listeners
  }

  subscribe(key: string, callback: Function): () => void {
    // Subscribe to state changes
    // Return unsubscribe function
  }

  handleServerMessage(resp: ActionResp): void {
    // Update state based on server response
    switch (Object.keys(resp)[0]) {
      case "ViewMsg": // Update room info
      case "CombatNormalMsg": // Update combat state
      case "MoveMsg": // Update location
      // ... etc
    }
  }
}
```

### 4.3 Message Parser (`message-parser.ts`)

```typescript
interface ParsedMessage {
  text: string;
  type: "movement" | "combat" | "dialogue" | "system" | "chat" | "error";
  timestamp: Date;
  metadata?: any;
}

class MessageParser {
  parse(resp: ActionResp): ParsedMessage {
    // Parse server response into display message
    // Extract relevant data for state updates
    // Return formatted message for display
  }

  formatCombatMessage(attacker: string, defender: string, desc: string, damage: number): ParsedMessage {
    return {
      text: `${attacker} attacks ${defender} with ${desc} for ${damage} damage!`,
      type: "combat",
      timestamp: new Date(),
      metadata: { attacker, defender, damage }
    };
  }

  // Similar methods for each ActionResp type
}
```

### 4.4 Component Example: MessageFeed (`components/message-feed.ts`)

```typescript
class MessageFeed {
  private container: HTMLElement;
  private maxMessages = 100;

  constructor(containerId: string, stateManager: GameStateManager) {
    this.container = document.getElementById(containerId)!;

    // Subscribe to message updates
    stateManager.subscribe("messages", (messages: ParsedMessage[]) => {
      this.render(messages);
    });
  }

  render(messages: ParsedMessage[]): void {
    // Clear and re-render messages
    // Apply color coding based on type
    // Auto-scroll to bottom
  }

  addMessage(message: ParsedMessage): void {
    // Append single message
    // Limit to maxMessages
  }
}
```

---

## 5. UI/UX Design Details

### 5.1 Color Scheme (Wuxia Theme)

```css
:root {
  /* Primary colors - Traditional Chinese */
  --color-bg-primary: #1a1a1a;      /* Dark background */
  --color-bg-secondary: #2a2a2a;    /* Panel backgrounds */
  --color-accent: #d4af37;          /* Gold accent */
  --color-accent-alt: #8b0000;      /* Dark red */

  /* Message types */
  --color-movement: #4a9eff;        /* Blue */
  --color-combat: #ff4444;          /* Red */
  --color-dialogue: #44ff44;        /* Green */
  --color-system: #888888;          /* Gray */
  --color-chat: #ffaa44;            /* Orange */

  /* UI elements */
  --color-hp: #ff3333;              /* HP bar */
  --color-ap: #3399ff;              /* AP bar */
  --color-border: #444444;
  --color-text: #e0e0e0;
}
```

### 5.2 Responsive Breakpoints

- **Desktop** (1024px+): 3-column layout (messages, center, stats)
- **Tablet** (768px-1023px): 2-column layout (messages + stats combined)
- **Mobile** (< 768px): Single column, tabbed interface

### 5.3 Keyboard Shortcuts

| Key | Action |
|-----|--------|
| WASD / Arrow Keys | Movement (N/W/S/E) |
| 1-9 | Quick skill activation |
| Enter | Focus command input |
| Esc | Close panels/cancel actions |
| ↑/↓ | Command history navigation |
| Tab | Cycle through NPCs in room |
| L | Look (refresh room view) |
| H | Help panel |

### 5.4 Accessibility Features

- ARIA labels for all interactive elements
- Keyboard navigation for all actions
- High contrast mode option
- Font size adjustment
- Screen reader friendly message announcements

---

## 6. Testing Strategy

### 6.1 Manual Testing Checklist

- [ ] Connection/disconnection handling
- [ ] Login with valid/invalid credentials
- [ ] Movement in all 8 directions
- [ ] Room view updates correctly
- [ ] NPC interaction (talk, attack)
- [ ] Combat flow (attack, damage, victory/defeat)
- [ ] Message display and scrolling
- [ ] Command history navigation
- [ ] State persistence across reconnections
- [ ] Error handling for invalid commands
- [ ] Multiple clients simultaneously

### 6.2 Browser Compatibility

- Chrome/Edge (Chromium) 90+
- Firefox 88+
- Safari 14+
- Mobile browsers (iOS Safari, Chrome Mobile)

---

## 7. Deployment

### 7.1 Build Process

```bash
npm run build
# Outputs to dist/ directory
# Static files ready for serving
```

### 7.2 Serving Options

**Option 1: Separate HTTP Server**
- Use nginx, Apache, or simple Python server
- Serve static files from `dist/`
- WebSocket connects to Haskell server on different port

**Option 2: Integrated with Haskell Server**
- Add HTTP file serving to Haskell server (using Warp/Servant)
- Serve client files from `/public` route
- Single server for both HTTP and WebSocket

**Option 3: GitHub Pages / Netlify**
- Host static client files on CDN
- Connect to remote WebSocket server
- CORS considerations needed

---

## 8. Future Enhancements

### 8.1 Advanced Features
- **Graphical mini-map** with room nodes and connections
- **Character avatar** customization
- **Sound effects** and ambient music
- **Animations** for combat actions
- **Rich text formatting** in descriptions (markdown support)
- **Macros** for command sequences
- **Logs export** for session replay
- **Mobile app** using Capacitor/Cordova

### 8.2 Social Features
- **Player list** showing online users
- **Private messaging** between players
- **Party system** for group activities
- **Leaderboards** for achievements
- **Profile pages** with character history

### 8.3 Developer Tools
- **Debug panel** showing raw messages
- **Performance metrics** (latency, message rate)
- **Mock server** for offline development
- **Message logger** for debugging

---

## 9. Estimated Timeline

| Phase | Duration | Complexity |
|-------|----------|------------|
| Phase 1: Infrastructure | 2-3 days | Medium |
| Phase 2: Core Interface | 3-4 days | Medium |
| Phase 3: Combat System | 2-3 days | Medium |
| Phase 4: Polish & UX | 2-3 days | Low |
| Phase 5: Advanced Features | 5-7 days | High |

**Total MVP (Phases 1-3):** ~7-10 days
**Full Featured (Phases 1-5):** ~14-20 days

---

## 10. Dependencies

### 10.1 NPM Packages

```json
{
  "devDependencies": {
    "typescript": "^5.0.0",
    "vite": "^4.3.0",
    "@types/node": "^20.0.0"
  },
  "dependencies": {
    // No runtime dependencies for minimal build
    // Optional: add if needed
    // "marked": "^5.0.0"  // For markdown support
  }
}
```

### 10.2 Browser APIs Used
- WebSocket API (native)
- LocalStorage API (native)
- Fetch API (native, if needed for REST endpoints)
- AudioContext API (optional, for sound effects)

---

## 11. Implementation Priority

**Must Have (MVP):**
1. Login screen
2. WebSocket connection
3. Message display
4. Movement commands
5. Room viewing
6. Basic combat interface
7. Command input

**Should Have:**
8. Direction pad buttons
9. NPC interaction buttons
10. Status panel with stats
11. Combat HP/AP bars
12. Message type color coding
13. Command history

**Nice to Have:**
14. Keyboard shortcuts
15. Sound effects
16. Animations
17. Help system
18. Settings persistence
19. Responsive design
20. Advanced combat UI

---

## 12. Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| WebSocket disconnections | High | Implement reconnection with exponential backoff |
| State desync | Medium | Periodic state refresh from server |
| Browser compatibility | Low | Use standard APIs, test on major browsers |
| Message parsing errors | Medium | Comprehensive error handling, fallback messages |
| Performance with many messages | Low | Message limiting, virtual scrolling |
| CORS issues | Medium | Configure server headers, same-origin deployment |

---

## Conclusion

This design provides a comprehensive, production-ready web client for the Haskell Wuxia MUD game. The phased approach allows for incremental development and testing, with Phase 1-3 delivering a fully functional MVP that enables basic gameplay.

The vanilla TypeScript approach keeps dependencies minimal while maintaining code quality and type safety. The modular component architecture allows for easy extension and maintenance as the game evolves.

**Next Steps:**
1. Review and approve this design
2. Set up development environment (Phase 1)
3. Implement MVP (Phases 1-3)
4. Test with actual Haskell server
5. Iterate based on user feedback
