// WebSocket MUD Client
class MUDClient {
    constructor() {
        this.ws = null;
        this.username = null;
        this.connected = false;
        this.inBattle = false;
        this.currentEnemy = null;
        this.playerStats = {
            hp: 100, maxHp: 100,
            qi: 100, maxQi: 100,
            ap: 0
        };
        this.skillCooldowns = {};
        this.activeEffects = [];
        this.soundEnabled = false;
        this.audioContext = null;

        // DOM Elements
        this.messageDisplay = document.getElementById('message-display');
        this.commandInput = document.getElementById('command-input');
        this.sendBtn = document.getElementById('send-btn');
        this.usernameInput = document.getElementById('username-input');
        this.loginBtn = document.getElementById('login-btn');
        this.connectionStatus = document.getElementById('connection-status');

        // Player status elements
        this.playerName = document.getElementById('player-name');
        this.playerLocation = document.getElementById('player-location');
        this.playerStatus = document.getElementById('player-status');

        // Resource bar elements
        this.hpBarFill = document.getElementById('hp-bar-fill');
        this.hpText = document.getElementById('hp-text');
        this.qiBarFill = document.getElementById('qi-bar-fill');
        this.qiText = document.getElementById('qi-text');
        this.apBarFill = document.getElementById('ap-bar-fill');
        this.apText = document.getElementById('ap-text');

        // Battle panel elements
        this.battlePanel = document.getElementById('battle-panel');
        this.battlePlayerName = document.getElementById('battle-player-name');
        this.battleEnemyName = document.getElementById('battle-enemy-name');
        this.battlePlayerHp = document.getElementById('battle-player-hp');
        this.battleEnemyHp = document.getElementById('battle-enemy-hp');

        // Room elements
        this.roomName = document.getElementById('room-name');
        this.roomDesc = document.getElementById('room-desc');
        this.roomCharacters = document.getElementById('room-characters');
        this.roomExits = document.getElementById('room-exits');

        // Effects
        this.activeEffectsDiv = document.getElementById('active-effects');

        // Combat log
        this.combatLog = document.getElementById('combat-log');
        this.combatLogToggle = document.getElementById('combat-log-toggle');

        this.initializeEventListeners();
    }

    initializeEventListeners() {
        // Login
        this.loginBtn.addEventListener('click', () => this.connect());
        this.usernameInput.addEventListener('keypress', (e) => {
            if (e.key === 'Enter') this.connect();
        });

        // Command input
        this.sendBtn.addEventListener('click', () => this.sendCommand());
        this.commandInput.addEventListener('keypress', (e) => {
            if (e.key === 'Enter') this.sendCommand();
        });

        // Action buttons
        document.querySelectorAll('.btn-action').forEach(btn => {
            btn.addEventListener('click', () => this.handleActionButton(btn));
        });

        // Skill cards
        document.querySelectorAll('.skill-card').forEach(card => {
            card.addEventListener('click', () => this.handleSkillClick(card));
        });

        // Combat log toggle
        this.combatLogToggle.addEventListener('click', () => {
            this.combatLog.classList.toggle('collapsed');
            const icon = this.combatLogToggle.querySelector('.toggle-icon');
            icon.textContent = this.combatLog.classList.contains('collapsed') ? '+' : '-';
        });

        // Help modal
        const helpBtn = document.getElementById('help-btn');
        const helpModal = document.getElementById('help-modal');
        const closeHelp = document.getElementById('close-help');

        if (helpBtn && helpModal) {
            helpBtn.addEventListener('click', () => {
                helpModal.classList.remove('hidden');
            });

            closeHelp.addEventListener('click', () => {
                helpModal.classList.add('hidden');
            });

            // Close modal when clicking outside
            helpModal.addEventListener('click', (e) => {
                if (e.target === helpModal) {
                    helpModal.classList.add('hidden');
                }
            });

            // Close modal with Escape key
            document.addEventListener('keydown', (e) => {
                if (e.key === 'Escape' && !helpModal.classList.contains('hidden')) {
                    helpModal.classList.add('hidden');
                }
            });
        }

        // Message filtering
        this.currentFilter = 'all';
        document.querySelectorAll('.filter-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                // Update active state
                document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
                btn.classList.add('active');

                // Apply filter
                this.currentFilter = btn.dataset.filter;
                this.applyMessageFilter();
            });
        });

        // Sound toggle
        const soundToggle = document.getElementById('sound-toggle');
        if (soundToggle) {
            soundToggle.addEventListener('change', (e) => {
                this.soundEnabled = e.target.checked;
                if (this.soundEnabled && !this.audioContext) {
                    this.audioContext = new (window.AudioContext || window.webkitAudioContext)();
                }
            });
        }
    }

    playSound(type) {
        if (!this.soundEnabled || !this.audioContext) return;

        const ctx = this.audioContext;
        const oscillator = ctx.createOscillator();
        const gainNode = ctx.createGain();

        oscillator.connect(gainNode);
        gainNode.connect(ctx.destination);

        // Different sounds for different events
        switch (type) {
            case 'attack':
                oscillator.frequency.value = 200;
                oscillator.type = 'square';
                gainNode.gain.value = 0.1;
                break;
            case 'skill':
                oscillator.frequency.value = 400;
                oscillator.type = 'sine';
                gainNode.gain.value = 0.15;
                break;
            case 'damage':
                oscillator.frequency.value = 150;
                oscillator.type = 'sawtooth';
                gainNode.gain.value = 0.1;
                break;
            case 'victory':
                oscillator.frequency.value = 600;
                oscillator.type = 'sine';
                gainNode.gain.value = 0.12;
                break;
            case 'defeat':
                oscillator.frequency.value = 100;
                oscillator.type = 'sine';
                gainNode.gain.value = 0.1;
                break;
            default:
                oscillator.frequency.value = 300;
                oscillator.type = 'sine';
                gainNode.gain.value = 0.1;
        }

        oscillator.start(ctx.currentTime);
        gainNode.gain.exponentialRampToValueAtTime(0.01, ctx.currentTime + 0.15);
        oscillator.stop(ctx.currentTime + 0.15);
    }

    applyMessageFilter() {
        const messages = this.messageDisplay.querySelectorAll('.message');
        messages.forEach(msg => {
            if (this.currentFilter === 'all') {
                msg.classList.remove('filtered');
            } else {
                // Determine message category
                const isCombat = msg.classList.contains('message-combat') || msg.classList.contains('message-skill');
                const isDialogue = msg.classList.contains('message-dialogue') || msg.classList.contains('message-say');
                const isSystem = msg.classList.contains('message-system') || msg.classList.contains('message-move') || msg.classList.contains('message-error');

                if (this.currentFilter === 'combat' && !isCombat) {
                    msg.classList.add('filtered');
                } else if (this.currentFilter === 'dialogue' && !isDialogue) {
                    msg.classList.add('filtered');
                } else if (this.currentFilter === 'system' && !isSystem) {
                    msg.classList.add('filtered');
                } else {
                    msg.classList.remove('filtered');
                }
            }
        });
    }

    connect() {
        const username = this.usernameInput.value.trim();
        if (!username) {
            this.displayMessage('Please enter a username', 'error');
            return;
        }

        this.username = username;
        this.displayMessage(`Connecting as ${username}...`, 'system');

        try {
            this.ws = new WebSocket('ws://127.0.0.1:9160');

            this.ws.onopen = () => {
                this.connected = true;
                this.updateConnectionStatus(true);
                this.displayMessage('Connected to server!', 'system');

                // Send login event as JSON with tag
                const loginEvent = JSON.stringify({
                    tag: "Login",
                    username: username,
                    password: ""
                });
                this.ws.send(loginEvent);

                // Enable UI
                this.commandInput.disabled = false;
                this.sendBtn.disabled = false;
                this.loginBtn.disabled = true;
                this.usernameInput.disabled = true;

                // Update player name
                this.playerName.textContent = username;
                this.battlePlayerName.textContent = username;

                // Request initial view
                setTimeout(() => {
                    this.sendAction({ other: "view" });
                }, 500);
            };

            this.ws.onmessage = (event) => {
                this.handleMessage(event.data);
            };

            this.ws.onerror = (error) => {
                this.displayMessage('Connection error!', 'error');
                console.error('WebSocket error:', error);
            };

            this.ws.onclose = () => {
                this.connected = false;
                this.updateConnectionStatus(false);
                this.displayMessage('Disconnected from server', 'system');

                // Disable UI
                this.commandInput.disabled = true;
                this.sendBtn.disabled = true;
                this.loginBtn.disabled = false;
                this.usernameInput.disabled = false;
            };

        } catch (error) {
            this.displayMessage(`Failed to connect: ${error.message}`, 'error');
        }
    }

    handleMessage(data) {
        try {
            // Try to parse as JSON first
            const message = JSON.parse(data);
            this.processGameMessage(message);
        } catch (e) {
            // If not JSON, display as plain text
            this.displayMessage(data, 'system');
        }
    }

    processGameMessage(message) {
        // Handle different message types based on the ActionResp structure
        // Server uses Generic-derived JSON with "tag" and "contents" fields

        if (typeof message === 'object') {
            const msgType = message.tag;
            const contents = message.contents;

            switch (msgType) {
                case 'MoveMsg':
                    this.handleMoveMsg(contents);
                    break;
                case 'ViewMsg':
                    this.handleViewMsg(contents);
                    break;
                case 'AttackMsg':
                    this.handleAttackMsg(contents);
                    break;
                case 'CombatNormalMsg':
                    this.handleCombatNormalMsg(contents);
                    break;
                case 'CombatSettlementMsg':
                    this.handleCombatSettlementMsg(contents);
                    break;
                case 'SkillMsg':
                    this.handleSkillMsg(contents);
                    break;
                case 'DialogueMsg':
                    this.handleDialogueMsg(contents);
                    break;
                case 'PlayerStatsMsg':
                    this.handlePlayerStatsMsg(contents);
                    break;
                case 'SayMsg':
                    this.handleSayMsg(contents);
                    break;
                case 'UseItemMsg':
                    this.handleUseItemMsg(contents);
                    break;
                default:
                    // Fallback: display as JSON string
                    this.displayMessage(JSON.stringify(message, null, 2), 'system');
            }
        }
    }

    handleMoveMsg(contents) {
        this.displayMessage(`You moved to: ${contents}`, 'move');
        this.playerLocation.textContent = contents;
    }

    handleViewMsg(contents) {
        const [roomName, roomDesc, chars, exits] = contents;

        // Update room panel
        this.roomName.textContent = roomName;
        this.roomDesc.textContent = roomDesc;
        this.playerLocation.textContent = roomName;

        // Clear and update characters list using safe DOM methods
        this.roomCharacters.textContent = '';
        if (chars && chars.length > 0) {
            chars.forEach(char => {
                // Parse character name and ID from "Name (id)" format
                const match = char.match(/^(.+)\s+\((.+)\)$/);
                const charDiv = document.createElement('div');
                charDiv.className = 'character-item';

                if (match) {
                    const [, name, id] = match;
                    charDiv.textContent = name;
                    charDiv.dataset.charId = id;
                    charDiv.addEventListener('click', () => this.quickInteract(id));
                } else {
                    charDiv.textContent = char;
                }
                this.roomCharacters.appendChild(charDiv);
            });
        } else {
            const emptySpan = document.createElement('span');
            emptySpan.className = 'empty-list';
            emptySpan.textContent = 'None';
            this.roomCharacters.appendChild(emptySpan);
        }

        // Clear and update exits list using safe DOM methods
        this.roomExits.textContent = '';
        if (exits && exits.length > 0) {
            exits.forEach(exit => {
                const exitDiv = document.createElement('div');
                exitDiv.className = 'exit-item';
                exitDiv.textContent = exit;
                exitDiv.addEventListener('click', () => this.goDirection(exit));
                this.roomExits.appendChild(exitDiv);
            });
        } else {
            const emptySpan = document.createElement('span');
            emptySpan.className = 'empty-list';
            emptySpan.textContent = 'None';
            this.roomExits.appendChild(emptySpan);
        }

        // Display in message area too
        let viewText = `\n[${roomName}]\n${roomDesc}`;
        if (chars && chars.length > 0) {
            viewText += `\nCharacters: ${chars.join(', ')}`;
        }
        if (exits && exits.length > 0) {
            viewText += `\nExits: ${exits.join(', ')}`;
        }
        this.displayMessage(viewText, 'system');
    }

    handleAttackMsg(contents) {
        const [attacker, defender] = contents;
        this.displayMessage(`${attacker} attacks ${defender}!`, 'combat');
        this.addCombatLog(`${attacker} attacks ${defender}!`);
        this.playSound('attack');

        // Enter battle mode
        this.setBattleMode(true, defender);
    }

    handleCombatNormalMsg(contents) {
        const [attacker, defender, skillMsg, damage] = contents;
        const msg = `${attacker} ${skillMsg} -> ${defender} (-${damage} HP)`;
        this.displayMessage(msg, 'combat');
        this.addCombatLog(msg);
        this.playSound('damage');
    }

    handleCombatSettlementMsg(contents) {
        const [player, enemy, won] = contents;
        const result = won ? 'Victory!' : 'Defeat';
        const msg = `${result} Battle with ${enemy} ended.`;
        this.displayMessage(msg, 'combat');
        this.addCombatLog(msg);
        this.playSound(won ? 'victory' : 'defeat');

        // Exit battle mode
        this.setBattleMode(false);
    }

    handleSkillMsg(contents) {
        const [caster, target, skillDesc] = contents;
        const msg = `${caster} -> ${target}: ${skillDesc}`;
        this.displayMessage(msg, 'skill');
        this.addCombatLog(msg);
        this.playSound('skill');
    }

    handleDialogueMsg(contents) {
        const [charName, dialogue] = contents;
        this.displayMessage(`${charName}: "${dialogue}"`, 'dialogue');
    }

    handlePlayerStatsMsg(contents) {
        const [hp, maxHp, qi, maxQi, ap, status] = contents;

        // Update stats
        this.playerStats = { hp, maxHp, qi, maxQi, ap };

        // Update resource bars
        this.updateResourceBars();

        // Update status
        this.playerStatus.textContent = status;

        // Update battle mode based on status
        if (status === 'In Battle' && !this.inBattle) {
            this.setBattleMode(true);
        } else if (status !== 'In Battle' && this.inBattle) {
            this.setBattleMode(false);
        }
    }

    handleSayMsg(contents) {
        const [speaker, message] = contents;
        this.displayMessage(`${speaker} says: "${message}"`, 'say');
    }

    handleUseItemMsg(contents) {
        const [user, itemDesc] = contents;
        this.displayMessage(`${user} uses ${itemDesc}`, 'system');
    }

    updateResourceBars() {
        const { hp, maxHp, qi, maxQi, ap } = this.playerStats;

        // HP bar
        const hpPercent = (hp / maxHp) * 100;
        this.hpBarFill.style.width = `${hpPercent}%`;
        this.hpText.textContent = `${hp}/${maxHp}`;

        // Qi bar
        const qiPercent = (qi / maxQi) * 100;
        this.qiBarFill.style.width = `${qiPercent}%`;
        this.qiText.textContent = `${qi}/${maxQi}`;

        // AP bar (max 100 for display purposes)
        const apPercent = Math.min(ap, 100);
        this.apBarFill.style.width = `${apPercent}%`;
        this.apText.textContent = Math.round(ap);

        // Update battle panel HP
        this.battlePlayerHp.style.width = `${hpPercent}%`;

        // Update skill availability based on resources
        this.updateSkillAvailability();
    }

    updateSkillAvailability() {
        const { qi, ap } = this.playerStats;

        document.querySelectorAll('.skill-card').forEach(card => {
            const skillId = card.dataset.skillId;
            const requires = card.dataset.requires;

            // Check if skill is locked due to missing buff
            if (requires) {
                const hasRequirement = this.activeEffects.some(e => e.id === requires);
                card.classList.toggle('skill-locked', !hasRequirement);
            }

            // Check cooldown
            if (this.skillCooldowns[skillId] && this.skillCooldowns[skillId] > 0) {
                card.classList.add('on-cooldown');
            } else {
                card.classList.remove('on-cooldown');
            }
        });
    }

    setBattleMode(active, enemyName = null) {
        this.inBattle = active;

        if (active) {
            this.battlePanel.classList.remove('hidden');
            if (enemyName) {
                this.currentEnemy = enemyName;
                this.battleEnemyName.textContent = enemyName;
                this.battleEnemyHp.style.width = '100%';
            }
        } else {
            this.battlePanel.classList.add('hidden');
            this.currentEnemy = null;
        }
    }

    addCombatLog(text) {
        const entry = document.createElement('div');
        entry.className = 'combat-log-entry';
        entry.textContent = `[${this.getTimeString()}] ${text}`;
        this.combatLog.appendChild(entry);
        this.combatLog.scrollTop = this.combatLog.scrollHeight;
    }

    sendCommand() {
        const command = this.commandInput.value.trim();
        if (!command || !this.connected) return;

        this.displayMessage(`> ${command}`, 'system');
        this.commandInput.value = '';

        // Parse command and convert to JSON
        const action = this.parseCommand(command);
        if (action) {
            this.sendAction(action);
        } else {
            this.displayMessage('Unknown command format', 'error');
        }
    }

    parseCommand(command) {
        const parts = command.toLowerCase().split(' ');
        const action = parts[0];
        const param = parts.slice(1).join(' ');

        // Direction mapping
        const dirMap = {
            'north': 'North', 'south': 'South', 'east': 'East', 'west': 'West',
            'northeast': 'NorthEast', 'northwest': 'NorthWest',
            'southeast': 'SouthEast', 'southwest': 'SouthWest',
            'ne': 'NorthEast', 'nw': 'NorthWest', 'se': 'SouthEast', 'sw': 'SouthWest',
            'n': 'North', 's': 'South', 'e': 'East', 'w': 'West'
        };

        switch (action) {
            case 'go':
            case 'move':
                return param ? { go: dirMap[param] || param } : null;
            case 'n':
            case 'north':
                return { go: 'North' };
            case 's':
            case 'south':
                return { go: 'South' };
            case 'e':
            case 'east':
                return { go: 'East' };
            case 'w':
            case 'west':
                return { go: 'West' };
            case 'attack':
                return param ? { attack: param } : null;
            case 'talk':
                return param ? { talk: param } : null;
            case 'perform':
            case 'skill':
            case 'cast':
                return param ? { perform: param } : null;
            case 'say':
                return param ? { say: param } : null;
            case 'use':
                return param ? { use: param } : null;
            case 'look':
            case 'view':
            case 'l':
                return { other: 'view' };
            default:
                return { other: command };
        }
    }

    sendAction(action) {
        if (this.connected && this.ws) {
            // Wrap action in NetPlayerAction NetEvent
            const netEvent = {
                tag: "NetPlayerAction",
                contents: action
            };
            const jsonMsg = JSON.stringify(netEvent);
            console.log('Sending:', jsonMsg);
            this.ws.send(jsonMsg);
        }
    }

    handleActionButton(btn) {
        if (!this.connected) return;

        const action = btn.dataset.action;
        let param = btn.dataset.param;
        const paramPrompt = btn.dataset.paramPrompt;

        // Capitalize directions for go command
        if (action === 'go' && param) {
            const dirMap = {
                'north': 'North', 'south': 'South', 'east': 'East', 'west': 'West',
                'northeast': 'NorthEast', 'northwest': 'NorthWest',
                'southeast': 'SouthEast', 'southwest': 'SouthWest'
            };
            param = dirMap[param.toLowerCase()] || param;
        }

        let actionObj;

        if (paramPrompt) {
            // Button needs user input
            const userInput = prompt(paramPrompt);
            if (!userInput) return;
            actionObj = { [action]: userInput };
        } else if (param) {
            // Button has predefined parameter
            actionObj = { [action]: param };
        } else if (action === 'view') {
            // Special case for view
            actionObj = { other: "view" };
        } else {
            actionObj = { [action]: "" };
        }

        this.sendAction(actionObj);
    }

    handleSkillClick(card) {
        if (!this.connected || !this.inBattle) {
            this.displayMessage('Skills can only be used in combat', 'error');
            return;
        }

        const skillId = card.dataset.skillId;

        // Check if on cooldown
        if (card.classList.contains('on-cooldown')) {
            this.displayMessage('Skill is on cooldown', 'error');
            return;
        }

        // Check if locked
        if (card.classList.contains('skill-locked')) {
            const requires = card.dataset.requires;
            this.displayMessage(`Requires ${requires} to be active`, 'error');
            return;
        }

        this.sendAction({ perform: skillId });
    }

    // Quick interact with character
    quickInteract(charId) {
        if (!this.connected) return;

        // Show simple menu
        const action = confirm(`Interact with ${charId}?\n\nOK = Talk\nCancel = Attack`);
        if (action) {
            this.sendAction({ talk: charId });
        } else {
            this.sendAction({ attack: charId });
        }
    }

    // Quick direction movement
    goDirection(direction) {
        if (!this.connected) return;
        this.sendAction({ go: direction });
    }

    displayMessage(text, type = 'system') {
        const messageDiv = document.createElement('div');
        messageDiv.className = `message message-${type}`;

        // Add timestamp
        const timeSpan = document.createElement('span');
        timeSpan.className = 'message-time';
        timeSpan.textContent = this.getTimeString();

        messageDiv.appendChild(timeSpan);
        messageDiv.appendChild(document.createTextNode(text));

        this.messageDisplay.appendChild(messageDiv);
        this.messageDisplay.scrollTop = this.messageDisplay.scrollHeight;
    }

    getTimeString() {
        const now = new Date();
        return now.toLocaleTimeString('en-US', {
            hour: '2-digit',
            minute: '2-digit',
            hour12: false
        });
    }

    updateConnectionStatus(connected) {
        if (connected) {
            this.connectionStatus.textContent = 'Connected';
            this.connectionStatus.className = 'status-connected';
        } else {
            this.connectionStatus.textContent = 'Disconnected';
            this.connectionStatus.className = 'status-disconnected';
        }
    }
}

// Initialize client when page loads
let mudClient;
document.addEventListener('DOMContentLoaded', () => {
    mudClient = new MUDClient();
    window.mudClient = mudClient;
});
