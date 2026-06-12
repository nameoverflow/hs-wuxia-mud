// WebSocket MUD Client
class MUDClient {
    constructor() {
        this.i18n = window.MUD_I18N || {};
        this.locale = this.getInitialLocale();
        this.ws = null;
        this.username = null;
        this.connected = false;
        this.inBattle = false;
        this.currentEnemy = null;
        this.playerStatusRaw = null;
        this.playerStats = {
            hp: 100, maxHp: 100,
            qi: 100, maxQi: 100,
            ap: 0
        };
        this.skillCooldowns = {};
        this.activeEffects = [];
        this.currentSkills = [];
        this.currentRoom = { name: null, desc: null, characters: [], exits: [] };
        this.questEntries = [];
        this.inventoryState = { money: 0, items: [] };
        this.lastStoryChoices = [];
        this.activeNpc = null;
        this.soundEnabled = false;
        this.audioContext = null;

        // DOM Elements
        this.messageDisplay = document.getElementById('message-display');
        this.commandInput = document.getElementById('command-input');
        this.sendBtn = document.getElementById('send-btn');
        this.usernameInput = document.getElementById('username-input');
        this.loginBtn = document.getElementById('login-btn');
        this.connectionStatus = document.getElementById('connection-status');

        // Story guide
        this.storyGuide = document.getElementById('story-guide');
        this.guideTitle = document.getElementById('guide-title');
        this.guideObjective = document.getElementById('guide-objective');
        this.guideActions = document.getElementById('guide-actions');

        // Player status elements
        this.playerName = document.getElementById('player-name');
        this.playerLocation = document.getElementById('player-location');
        this.playerStatus = document.getElementById('player-status');
        this.playerMoney = document.getElementById('player-money');

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
        this.roomMap = document.getElementById('room-map');

        // Effects
        this.activeEffectsDiv = document.getElementById('active-effects');

        // NPC modal
        this.npcModal = document.getElementById('npc-modal');
        this.npcModalName = document.getElementById('npc-modal-name');
        this.npcModalDesc = document.getElementById('npc-modal-desc');
        this.npcModalActions = document.getElementById('npc-modal-actions');
        this.closeNpcModalBtn = document.getElementById('close-npc-modal');

        // Quest and inventory
        this.questLog = document.getElementById('quest-log');
        this.inventoryList = document.getElementById('inventory-list');

        // Combat log
        this.combatLog = document.getElementById('combat-log');
        this.combatLogToggle = document.getElementById('combat-log-toggle');

        this.initializeEventListeners();
        this.applyI18n();
        this.updateConnectionStatus(false);
        this.updateStoryGuide();
    }

    getInitialLocale() {
        const saved = localStorage.getItem('mud-locale');
        if (saved === 'en' || saved === 'zh') return saved;
        return navigator.language && navigator.language.toLowerCase().startsWith('zh') ? 'zh' : 'en';
    }

    hasTranslation(key) {
        return Boolean((this.i18n[this.locale] && this.i18n[this.locale][key]) || (this.i18n.en && this.i18n.en[key]));
    }

    t(key, vars = {}) {
        const table = this.i18n[this.locale] || {};
        const fallback = this.i18n.en || {};
        let text = table[key] || fallback[key] || key;
        Object.entries(vars).forEach(([name, value]) => {
            text = text.replaceAll(`{${name}}`, String(value));
        });
        return text;
    }

    resolveI18nVars(raw) {
        if (!raw) return {};
        try {
            const parsed = JSON.parse(raw);
            return Object.fromEntries(
                Object.entries(parsed).map(([key, value]) => {
                    if (key.endsWith('Key')) {
                        return [key.slice(0, -3), this.t(value)];
                    }
                    return [key, value];
                })
            );
        } catch (error) {
            console.warn('Invalid i18n vars:', raw, error);
            return {};
        }
    }

    setLocale(locale) {
        if (locale !== 'en' && locale !== 'zh') return;
        if (this.locale === locale) return;
        this.locale = locale;
        localStorage.setItem('mud-locale', locale);
        this.applyI18n();
        this.refreshLocalizedState();
    }

    applyI18n() {
        document.documentElement.lang = this.locale === 'zh' ? 'zh-CN' : 'en';
        document.title = this.t('app.title');

        document.querySelectorAll('[data-i18n]').forEach(element => {
            element.textContent = this.t(element.dataset.i18n, this.resolveI18nVars(element.dataset.i18nVars));
        });

        document.querySelectorAll('[data-i18n-placeholder]').forEach(element => {
            element.setAttribute('placeholder', this.t(element.dataset.i18nPlaceholder));
        });

        document.querySelectorAll('[data-i18n-title]').forEach(element => {
            element.setAttribute('title', this.t(element.dataset.i18nTitle));
        });

        document.querySelectorAll('[data-i18n-aria-label]').forEach(element => {
            element.setAttribute('aria-label', this.t(element.dataset.i18nAriaLabel));
        });

        document.querySelectorAll('.locale-btn').forEach(button => {
            const active = button.dataset.locale === this.locale;
            button.classList.toggle('active', active);
            button.setAttribute('aria-pressed', String(active));
        });
    }

    refreshLocalizedState() {
        this.updateConnectionStatus(this.connected);
        if (this.playerStatusRaw !== null) {
            this.playerStatus.textContent = this.formatPlayerStatus(this.playerStatusRaw);
        }
        if (!this.username) {
            this.battlePlayerName.textContent = this.t('battle.player');
        }
        if (!this.currentEnemy) {
            this.battleEnemyName.textContent = this.t('battle.enemy');
        }
        this.renderRoom();
        this.renderActiveEffects();
        this.renderQuestLog();
        this.renderInventory();
        if (this.currentSkills.length > 0) {
            this.renderSkillPanel(this.currentSkills);
        }
        if (this.activeNpc) {
            this.renderNpcModal(this.activeNpc);
        }
        this.updateSkillAvailability();
        this.updateStoryGuide();
    }

    initializeEventListeners() {
        document.querySelectorAll('.locale-btn').forEach(button => {
            button.addEventListener('click', () => this.setLocale(button.dataset.locale));
        });

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

        if (this.npcModal) {
            this.npcModal.addEventListener('click', (event) => {
                if (event.target === this.npcModal) {
                    this.closeNpcModal();
                }
            });
        }

        if (this.closeNpcModalBtn) {
            this.closeNpcModalBtn.addEventListener('click', () => this.closeNpcModal());
        }

        document.addEventListener('keydown', (event) => {
            if (event.key === 'Escape' && this.npcModal && !this.npcModal.classList.contains('hidden')) {
                this.closeNpcModal();
            }
        });

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
                const isDialogue = msg.classList.contains('message-dialogue') || msg.classList.contains('message-story') || msg.classList.contains('message-say');
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
                this.updateStoryGuide();

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
                case 'BattleStateMsg':
                    this.handleBattleStateMsg(contents);
                    break;
                case 'SkillFailureMsg':
                    this.handleSkillFailureMsg(contents);
                    break;
                case 'StoryMsg':
                    this.handleStoryMsg(contents);
                    break;
                case 'QuestLogMsg':
                    this.handleQuestLogMsg(contents);
                    break;
                case 'InventoryMsg':
                    this.handleInventoryMsg(contents);
                    break;
                case 'RewardMsg':
                    this.handleRewardMsg(contents);
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
        const parsedChars = (chars || []).map(char => {
            if (char && typeof char === 'object') {
                const actions = Array.isArray(char.actions) ? char.actions : [];
                return {
                    name: char.name || char.id || 'Unknown',
                    id: char.id || null,
                    desc: char.desc || '',
                    actions: actions.map(action => String(action).toLowerCase())
                };
            }

            const text = String(char);
            const match = text.match(/^(.+)\s+\((.+)\)$/);
            return match
                ? { name: match[1], id: match[2], desc: '', actions: ['talk', 'attack'] }
                : { name: text, id: null, desc: '', actions: [] };
        });

        this.currentRoom = {
            name: roomName,
            desc: roomDesc,
            characters: parsedChars,
            exits: this.normalizeExitSummaries(exits || [])
        };

        this.renderRoom();
        this.updateStoryGuide();
        this.displayViewMessage();
    }

    renderRoom() {
        if (!this.roomName || !this.roomDesc || !this.roomCharacters || !this.roomExits) return;
        const { name, desc, characters, exits } = this.currentRoom;

        if (this.activeNpc && !characters.some(char => char.id === this.activeNpc.id)) {
            this.closeNpcModal();
        }

        this.roomName.textContent = name || '-';
        this.roomDesc.textContent = desc || '-';
        if (name) {
            this.playerLocation.textContent = name;
        }

        this.renderRoomMap(exits);

        this.roomCharacters.textContent = '';
        if (characters.length > 0) {
            characters.forEach(char => {
                const charDiv = document.createElement('div');
                charDiv.className = 'character-item';

                const charName = document.createElement('div');
                charName.className = 'character-name';
                charName.textContent = char.name;
                charDiv.appendChild(charName);

                if (char.id) {
                    charDiv.dataset.charId = char.id;
                    charDiv.classList.add('character-clickable');
                    charDiv.setAttribute('role', 'button');
                    charDiv.tabIndex = 0;
                    charDiv.addEventListener('click', () => this.openNpcModal(char));
                    charDiv.addEventListener('keydown', (event) => {
                        if (event.key === 'Enter' || event.key === ' ') {
                            event.preventDefault();
                            this.openNpcModal(char);
                        }
                    });
                }
                this.roomCharacters.appendChild(charDiv);
            });
        } else {
            const emptySpan = document.createElement('span');
            emptySpan.className = 'empty-list';
            emptySpan.textContent = this.t('ui.none');
            this.roomCharacters.appendChild(emptySpan);
        }

        this.roomExits.textContent = '';
        if (exits.length > 0) {
            exits.forEach(exit => {
                const direction = this.getExitDirection(exit);
                const label = this.getExitLabel(exit);
                if (!direction) return;
                const exitDiv = document.createElement('div');
                exitDiv.className = 'exit-item';
                exitDiv.textContent = label;
                exitDiv.title = this.t('map.move_to', { value: label });
                exitDiv.addEventListener('click', () => this.goDirection(direction));
                this.roomExits.appendChild(exitDiv);
            });
        } else {
            const emptySpan = document.createElement('span');
            emptySpan.className = 'empty-list';
            emptySpan.textContent = this.t('ui.none');
            this.roomExits.appendChild(emptySpan);
        }
    }

    openNpcModal(char) {
        if (!char || !this.npcModal) return;
        this.activeNpc = char;
        this.renderNpcModal(char);
        this.npcModal.classList.remove('hidden');
    }

    closeNpcModal() {
        if (!this.npcModal) return;
        this.npcModal.classList.add('hidden');
        this.activeNpc = null;
    }

    renderNpcModal(char) {
        if (!this.npcModalName || !this.npcModalDesc || !this.npcModalActions) return;

        this.npcModalName.textContent = char.name || '-';
        this.npcModalDesc.textContent = char.desc || '';
        this.npcModalDesc.classList.toggle('hidden', !char.desc);
        this.npcModalActions.textContent = '';

        const actions = new Set(char.actions || []);
        let actionCount = 0;

        if (actions.has('talk') || actions.has('dialogue')) {
            this.npcModalActions.appendChild(
                this.createNpcActionButton(this.t('action.talk'), 'talk', () => {
                    this.closeNpcModal();
                    this.sendAction({ talk: char.id });
                })
            );
            actionCount += 1;
        }

        if (actions.has('sparring')) {
            this.npcModalActions.appendChild(
                this.createNpcActionButton(this.t('action.spar'), 'spar', null, this.t('npc.action_unavailable'))
            );
            actionCount += 1;
        }

        if (actions.has('attack') || actions.has('attacking')) {
            this.npcModalActions.appendChild(
                this.createNpcActionButton(this.t('action.attack'), 'attack', () => {
                    this.closeNpcModal();
                    this.sendAction({ attack: char.id });
                })
            );
            actionCount += 1;
        }

        if (actionCount === 0) {
            const empty = document.createElement('span');
            empty.className = 'npc-no-actions';
            empty.textContent = this.t('npc.no_actions');
            this.npcModalActions.appendChild(empty);
        }
    }

    createNpcActionButton(label, kind, onClick, disabledReason = null) {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `npc-action npc-action-${kind}`;
        button.textContent = label;

        if (disabledReason) {
            button.disabled = true;
            button.title = disabledReason;
            button.setAttribute('aria-label', `${label}: ${disabledReason}`);
        } else if (onClick) {
            button.addEventListener('click', onClick);
        }

        return button;
    }

    renderRoomMap(exits) {
        if (!this.roomMap) return;

        this.roomMap.textContent = '';
        const currentNode = document.createElement('div');
        currentNode.className = 'map-node map-node-current';
        currentNode.textContent = this.currentRoom.name || this.t('map.current_position');
        this.roomMap.appendChild(currentNode);

        if (exits.length === 0) {
            const empty = document.createElement('div');
            empty.className = 'map-empty';
            empty.textContent = this.t('map.no_exits');
            this.roomMap.appendChild(empty);
            return;
        }

        exits.forEach(exit => {
            const direction = this.getExitDirection(exit);
            const label = this.getExitLabel(exit);
            if (!direction) return;

            const link = document.createElement('span');
            link.className = `map-link map-link-${direction}`;
            this.roomMap.appendChild(link);

            const node = document.createElement('button');
            node.type = 'button';
            node.className = `map-node map-node-exit map-node-${direction}`;
            node.dataset.direction = direction;
            node.textContent = label;
            node.title = this.t('map.move_to', { value: label });
            node.setAttribute('aria-label', this.t('map.move_to', { value: label }));
            node.disabled = !this.connected;
            node.addEventListener('click', () => this.goDirection(direction));
            this.roomMap.appendChild(node);
        });
    }

    displayViewMessage() {
        const { name, desc, characters, exits } = this.currentRoom;
        if (!name) return;
        let viewText = `\n[${name}]\n${desc || ''}`;
        if (characters.length > 0) {
            const characterText = characters
                .map(char => char.id ? `${char.name} (${char.id})` : char.name)
                .join(', ');
            viewText += `\n${this.t('view.characters')}: ${characterText}`;
        }
        if (exits.length > 0) {
            viewText += `\n${this.t('view.exits')}: ${exits.map(exit => this.getExitLabel(exit)).join(', ')}`;
        }
        this.displayMessage(viewText, 'system');
    }

    handleAttackMsg(contents) {
        const [attacker, defender] = contents;
        const msg = `${attacker} attacks ${defender}!`;
        this.displayMessage(msg, 'combat');
        this.addCombatLog(msg);
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

    handleSkillFailureMsg(contents) {
        this.displayMessage(contents, 'error');
        this.addCombatLog(`Skill failed: ${contents}`);
    }

    handleDialogueMsg(contents) {
        const [charName, dialogue] = contents;
        this.displayMessage(`${charName}: "${dialogue}"`, 'dialogue');
    }

    handleStoryMsg(contents) {
        const [speaker, text, choices] = contents;
        this.lastStoryChoices = choices || [];
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message message-story';

        const timeSpan = document.createElement('span');
        timeSpan.className = 'message-time';
        timeSpan.textContent = this.getTimeString();
        messageDiv.appendChild(timeSpan);

        const speakerSpan = document.createElement('span');
        speakerSpan.className = 'story-speaker';
        speakerSpan.textContent = `${speaker}: `;
        messageDiv.appendChild(speakerSpan);

        const textSpan = document.createElement('span');
        textSpan.className = 'story-text';
        textSpan.textContent = text;
        messageDiv.appendChild(textSpan);

        if (choices && choices.length > 0) {
            const choicesDiv = document.createElement('div');
            choicesDiv.className = 'story-choices';

            choices.forEach(choice => {
                const button = document.createElement('button');
                button.type = 'button';
                button.className = 'story-choice-btn';
                button.textContent = choice.storyChoiceRespText;
                button.title = `choose ${choice.storyChoiceRespId}`;
                button.addEventListener('click', () => {
                    this.sendAction({ choose: choice.storyChoiceRespId });
                    this.lastStoryChoices = [];
                    this.updateStoryGuide();
                    choicesDiv.querySelectorAll('.story-choice-btn').forEach(btn => {
                        btn.disabled = true;
                    });
                });
                choicesDiv.appendChild(button);
            });

            messageDiv.appendChild(choicesDiv);
        }

        this.messageDisplay.appendChild(messageDiv);
        this.messageDisplay.scrollTop = this.messageDisplay.scrollHeight;
        this.applyMessageFilter();
        this.updateStoryGuide();
    }

    handleQuestLogMsg(entries) {
        this.questEntries = entries || [];
        this.renderQuestLog();
        this.updateStoryGuide();
    }

    renderQuestLog() {
        this.questLog.textContent = '';
        if (!this.questEntries || this.questEntries.length === 0) {
            const empty = document.createElement('span');
            empty.className = 'empty-list';
            empty.textContent = this.t('ui.none');
            this.questLog.appendChild(empty);
            return;
        }

        this.questEntries.forEach(entry => {
            const item = document.createElement('div');
            item.className = 'quest-entry';
            if (entry.questLogEntryCompleted) {
                item.classList.add('completed');
            }

            const title = document.createElement('div');
            title.className = 'quest-title';
            title.textContent = this.formatQuestName(entry);
            item.appendChild(title);

            const stage = document.createElement('div');
            stage.className = 'quest-stage';
            stage.textContent = this.formatQuestStage(entry);
            item.appendChild(stage);

            const objectiveText = this.formatQuestObjective(entry);
            if (objectiveText) {
                const objective = document.createElement('div');
                objective.className = 'quest-objective';
                objective.textContent = objectiveText;
                item.appendChild(objective);
            }

            const rewards = entry.questLogEntryRewards || [];
            if (rewards.length > 0) {
                const reward = document.createElement('div');
                reward.className = 'quest-reward';
                reward.textContent = this.t('quest.reward', { value: rewards.map(r => this.formatReward(r)).join(', ') });
                item.appendChild(reward);
            }

            this.questLog.appendChild(item);
        });
    }

    handleInventoryMsg(contents) {
        const [money, items] = contents;
        this.inventoryState = { money, items: items || [] };
        this.playerMoney.textContent = money;
        this.renderInventory();
    }

    renderInventory() {
        const items = this.inventoryState.items || [];
        this.inventoryList.textContent = '';

        if (items.length === 0) {
            const empty = document.createElement('span');
            empty.className = 'empty-list';
            empty.textContent = this.t('ui.none');
            this.inventoryList.appendChild(empty);
            return;
        }

        items.forEach(invItem => {
            const item = document.createElement('div');
            item.className = 'inventory-entry';

            const title = document.createElement('div');
            title.className = 'inventory-title';
            title.textContent = this.formatItemName(invItem.inventoryItemSummaryId, invItem.inventoryItemSummaryName);
            item.appendChild(title);

            const count = document.createElement('div');
            count.className = 'inventory-count';
            count.textContent = `x${invItem.inventoryItemSummaryAmount}`;
            item.appendChild(count);

            if (invItem.inventoryItemSummaryUsable) {
                const useButton = document.createElement('button');
                useButton.type = 'button';
                useButton.className = 'inventory-use-btn';
                useButton.textContent = this.t('action.use');
                useButton.disabled = !this.connected || this.inBattle;
                useButton.addEventListener('click', () => {
                    this.sendAction({ use: invItem.inventoryItemSummaryId });
                });
                item.appendChild(useButton);
            }

            this.inventoryList.appendChild(item);
        });
    }

    handleRewardMsg(rewards) {
        if (!rewards || rewards.length === 0) return;
        this.displayMessage(this.t('quest.reward', { value: rewards.map(r => this.formatReward(r)).join(', ') }), 'system');
    }

    handlePlayerStatsMsg(contents) {
        const [hp, maxHp, qi, maxQi, ap, status] = contents;

        // Update stats
        this.playerStats = { hp, maxHp, qi, maxQi, ap };

        // Update resource bars
        this.updateResourceBars();

        // Update status
        this.playerStatusRaw = status;
        this.playerStatus.textContent = this.formatPlayerStatus(status);

        // Update battle mode based on status
        if (status === 'In Battle' && !this.inBattle) {
            this.setBattleMode(true);
        } else if (status !== 'In Battle' && this.inBattle) {
            this.setBattleMode(false);
        }
        this.updateStoryGuide();
    }

    handleBattleStateMsg(snapshot) {
        const player = snapshot.battleSnapshotPlayer;
        const enemy = snapshot.battleSnapshotEnemy;
        const cooldowns = snapshot.battleSnapshotCooldowns || [];
        const skills = snapshot.battleSnapshotSkills || [];

        this.playerStats = {
            hp: player.combatantSnapshotHp,
            maxHp: player.combatantSnapshotMaxHp,
            qi: player.combatantSnapshotQi,
            maxQi: player.combatantSnapshotMaxQi,
            ap: player.combatantSnapshotAp
        };

        this.activeEffects = (player.combatantSnapshotEffects || []).map(effect => ({
            id: effect.effectSummaryId,
            name: effect.effectSummaryName,
            type: effect.effectSummaryType,
            remaining: effect.effectSummaryRemaining,
            value: effect.effectSummaryValue
        }));

        this.skillCooldowns = {};
        cooldowns.forEach(cd => {
            this.skillCooldowns[cd.cooldownSummarySkillId] = cd.cooldownSummaryRemaining;
        });

        if (skills.length > 0) {
            this.currentSkills = skills;
            this.renderSkillPanel(skills);
        }

        this.setBattleMode(true, enemy.combatantSnapshotName);
        this.updateResourceBars();
        this.updateBattleEnemy(enemy);
        this.renderActiveEffects();
        this.updateSkillAvailability();
        this.updateStoryGuide();
    }

    handleSayMsg(contents) {
        const [speaker, message] = contents;
        this.displayMessage(`${speaker} says: "${message}"`, 'say');
    }

    handleUseItemMsg(contents) {
        const [, itemDesc] = contents;
        this.displayMessage(itemDesc, 'system');
    }

    updateResourceBars() {
        const { hp, maxHp, qi, maxQi, ap } = this.playerStats;

        // HP bar
        const hpPercent = this.percent(hp, maxHp);
        this.hpBarFill.style.width = `${hpPercent}%`;
        this.hpText.textContent = `${hp}/${maxHp}`;

        // Qi bar
        const qiPercent = this.percent(qi, maxQi);
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
            const requires = (card.dataset.requires || '').split(',').filter(Boolean);
            const cost = Number(card.dataset.cost || 0);
            const apReq = Number(card.dataset.apReq || 0);

            // Check if skill is locked due to missing buff
            const hasRequirements = requires.every(req => this.activeEffects.some(e => e.id === req));
            const hasResources = qi >= cost && ap >= apReq;
            card.classList.toggle('skill-locked', !hasRequirements || !hasResources);

            // Check cooldown
            const remaining = this.skillCooldowns[skillId] || 0;
            card.classList.toggle('on-cooldown', remaining > 0);
            card.classList.toggle('skill-ready', hasRequirements && hasResources && remaining <= 0);
            const cooldownBar = card.querySelector('.skill-cooldown');
            if (cooldownBar) {
                const cooldown = Number(card.dataset.cooldown || remaining || 1);
                cooldownBar.style.width = remaining > 0 ? `${this.percent(remaining, cooldown)}%` : '0%';
            }

            const status = card.querySelector('.skill-status');
            if (status) {
                if (remaining > 0) {
                    status.textContent = this.t('skill.status.cd', { seconds: Math.ceil(remaining) });
                } else if (!hasRequirements) {
                    status.textContent = this.t('skill.status.need', { value: this.getRequirementLabels(card, requires).join(', ') });
                } else if (qi < cost) {
                    status.textContent = this.t('skill.status.qi', { current: qi, required: cost });
                } else if (ap < apReq) {
                    status.textContent = this.t('skill.status.ap', { current: Math.round(ap), required: apReq });
                } else {
                    status.textContent = this.t('skill.status.ready');
                }
            }
        });
    }

    updateBattleEnemy(enemy) {
        const hpPercent = this.percent(enemy.combatantSnapshotHp, enemy.combatantSnapshotMaxHp);
        this.battleEnemyName.textContent = enemy.combatantSnapshotName;
        this.battleEnemyHp.style.width = `${hpPercent}%`;
    }

    renderActiveEffects() {
        this.activeEffectsDiv.textContent = '';
        if (!this.activeEffects.length) {
            const empty = document.createElement('span');
            empty.className = 'no-effects';
            empty.textContent = this.t('ui.none');
            this.activeEffectsDiv.appendChild(empty);
            return;
        }

        this.activeEffects.forEach(effect => {
            const tag = document.createElement('span');
            tag.className = `effect-tag ${effect.type === 'debuff' || effect.type === 'dot' ? 'debuff' : 'buff'}`;
            const remaining = Math.max(0, Math.ceil(effect.remaining));
            tag.textContent = `${effect.name || effect.id} ${remaining}s`;
            this.activeEffectsDiv.appendChild(tag);
        });
    }

    renderSkillPanel(skills) {
        const panel = document.getElementById('skills-panel');
        panel.textContent = '';

        skills.forEach(skill => {
            const card = document.createElement('div');
            card.className = 'skill-card';
            if ((skill.skillSummaryDamage || 0) >= 100) {
                card.classList.add('skill-ultimate');
            }
            card.dataset.skillId = skill.skillSummaryId;
            card.dataset.cost = String(skill.skillSummaryCost);
            card.dataset.apReq = String(skill.skillSummaryApReq);
            card.dataset.cooldown = String(skill.skillSummaryCooldown);
            card.dataset.requires = (skill.skillSummaryReqStatus || []).join(',');
            card.dataset.requiresLabel = (skill.skillSummaryReqStatusNames || []).join(',');
            card.title = skill.skillSummaryDesc || '';

            const header = document.createElement('div');
            header.className = 'skill-header';
            const name = document.createElement('span');
            name.className = 'skill-name';
            name.textContent = this.formatSkillName(skill.skillSummaryId, skill.skillSummaryName);
            const cost = document.createElement('span');
            cost.className = 'skill-cost';
            cost.textContent = this.t('skill.cost.qi', { value: skill.skillSummaryCost });
            header.appendChild(name);
            header.appendChild(cost);

            const info = document.createElement('div');
            info.className = 'skill-info';
            const ap = document.createElement('span');
            ap.className = 'skill-ap';
            ap.textContent = this.t('skill.ap', { value: skill.skillSummaryApReq });
            info.appendChild(ap);
            if (skill.skillSummaryDamage !== null && skill.skillSummaryDamage !== undefined) {
                const damage = document.createElement('span');
                damage.className = 'skill-damage';
                damage.textContent = this.t('skill.damage', { value: skill.skillSummaryDamage });
                info.appendChild(damage);
            }
            if (skill.skillSummaryHeal !== null && skill.skillSummaryHeal !== undefined) {
                const heal = document.createElement('span');
                heal.className = 'skill-heal';
                heal.textContent = this.t('skill.heal', { value: skill.skillSummaryHeal });
                info.appendChild(heal);
            }

            card.appendChild(header);
            card.appendChild(info);

            if (skill.skillSummaryReqStatus && skill.skillSummaryReqStatus.length > 0) {
                const requirement = document.createElement('div');
                requirement.className = 'skill-requirement';
                requirement.textContent = this.t('skill.requires', {
                    value: (skill.skillSummaryReqStatusNames || skill.skillSummaryReqStatus.map(req => this.formatEffectName(req))).join(', ')
                });
                card.appendChild(requirement);
            }

            const status = document.createElement('div');
            status.className = 'skill-status';
            status.textContent = this.t('skill.status.waiting');
            card.appendChild(status);

            const cooldown = document.createElement('div');
            cooldown.className = 'skill-cooldown';
            card.appendChild(cooldown);

            card.addEventListener('click', () => this.handleSkillClick(card));
            panel.appendChild(card);
        });
    }

    percent(value, max) {
        if (!max || max <= 0) return 0;
        return Math.max(0, Math.min(100, (value / max) * 100));
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
            this.activeEffects = [];
            this.skillCooldowns = {};
            this.renderActiveEffects();
            this.updateSkillAvailability();
        }
        this.updateStoryGuide();
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
            case 'choose':
            case 'choice':
                return param ? { choose: param } : null;
            case 'use':
                return param ? { use: param } : null;
            case 'look':
            case 'view':
            case 'l':
                return { other: 'view' };
            case 'quests':
            case 'quest':
            case 'q':
                return { other: 'quests' };
            case 'inventory':
            case 'inv':
            case 'i':
                return { other: 'inventory' };
            default:
                return { other: command };
        }
    }

    formatReward(reward) {
        if (reward.rewardSummaryKind === 'money') {
            return `${reward.rewardSummaryAmount} ${reward.rewardSummaryName}`;
        }
        if (reward.rewardSummaryKind === 'martial_art') {
            return reward.rewardSummaryName;
        }
        return `${this.formatItemName(reward.rewardSummaryId, reward.rewardSummaryName)} x${reward.rewardSummaryAmount}`;
    }

    formatItemName(itemId, fallback) {
        return fallback;
    }

    formatSkillName(skillId, fallback) {
        return fallback;
    }

    formatEffectName(effectId) {
        return effectId;
    }

    getRequirementLabels(card, requires) {
        const explicit = (card.dataset.requiresLabel || '').split(',').map(label => label.trim()).filter(Boolean);
        return requires.map((req, index) => explicit[index] || this.formatEffectName(req));
    }

    formatQuestName(entry) {
        return entry.questLogEntryName;
    }

    formatQuestStage(entry) {
        if (entry.questLogEntryCompleted) return this.t('quest.completed');
        return entry.questLogEntryStage;
    }

    formatQuestObjective(entry) {
        return entry.questLogEntryObjective;
    }

    formatPlayerStatus(status) {
        const map = {
            'Normal': 'player_status.normal',
            'In Battle': 'player_status.in_battle',
            'Dead': 'player_status.dead',
            'Banned': 'player_status.banned'
        };
        return this.t(map[status] || 'player_status.unknown');
    }

    formatDirection(direction) {
        return this.t(`direction.${this.normalizeDirection(direction)}`);
    }

    normalizeDirection(direction) {
        const dir = String(direction || '');
        const map = {
            north: 'North',
            south: 'South',
            east: 'East',
            west: 'West',
            northeast: 'NorthEast',
            northwest: 'NorthWest',
            southeast: 'SouthEast',
            southwest: 'SouthWest'
        };
        return map[dir.toLowerCase()] || dir;
    }

    normalizeExitSummaries(exits) {
        return (exits || [])
            .map(exit => {
                if (typeof exit === 'string') {
                    return {
                        direction: this.normalizeDirection(exit),
                        roomId: null,
                        roomName: null,
                        position: null
                    };
                }

                if (!exit || typeof exit !== 'object') return null;
                return {
                    direction: this.normalizeDirection(exit.direction || exit.roomExitSummaryDirection),
                    roomId: exit.roomId || exit.roomExitSummaryRoomId || null,
                    roomName: exit.roomName || exit.roomExitSummaryRoomName || null,
                    position: exit.position || exit.roomExitSummaryPosition || null
                };
            })
            .filter(exit => exit && exit.direction);
    }

    getExitDirection(exit) {
        if (typeof exit === 'string') return this.normalizeDirection(exit);
        return this.normalizeDirection(exit && exit.direction);
    }

    getExitLabel(exit) {
        if (exit && typeof exit === 'object' && exit.roomName) return exit.roomName;
        return this.formatDirection(this.getExitDirection(exit));
    }

    updateStoryGuide() {
        if (!this.guideTitle || !this.guideObjective || !this.guideActions) return;

        this.guideActions.textContent = '';
        const quest = this.questEntries.find(entry => entry.questLogEntryId === 'cold_rain_inn');
        this.guideTitle.textContent = quest ? this.formatQuestName(quest) : '冷雨客栈';

        if (!this.connected) {
            this.guideObjective.textContent = this.t('guide.start_after_connect');
            return;
        }

        if (this.lastStoryChoices.length > 0) {
            this.guideObjective.textContent = this.t('guide.make_choice');
            this.lastStoryChoices.forEach(choice => {
                this.guideActions.appendChild(
                    this.createGuideAction(choice.storyChoiceRespText, { choose: choice.storyChoiceRespId }, 'choice')
                );
            });
            return;
        }

        if (this.inBattle) {
            const readySkill = this.getReadySkill();
            this.guideObjective.textContent = this.currentEnemy
                ? this.t('guide.defeat_enemy', { enemy: this.currentEnemy })
                : this.t('guide.defeat_current_enemy');
            if (readySkill) {
                this.guideActions.appendChild(
                    this.createGuideAction(
                        this.t('guide.use_skill', { skill: this.formatSkillName(readySkill.skillSummaryId, readySkill.skillSummaryName) }),
                        { perform: readySkill.skillSummaryId },
                        'combat'
                    )
                );
            } else {
                this.guideActions.appendChild(this.createGuideNote(this.t('guide.wait_ap')));
            }
            return;
        }

        if (!quest) {
            this.guideObjective.textContent = this.currentRoom.name ? '雨夜渡口，先问掌柜。' : '等待房间信息。';
            if (this.hasCharacter('cold_rain_innkeeper')) {
                this.guideActions.appendChild(
                    this.createGuideAction('询问冷雨掌柜', { talk: 'cold_rain_innkeeper' }, 'talk')
                );
            }
            return;
        }

        this.guideObjective.textContent = this.formatQuestObjective(quest) || quest.questLogEntryStage;

        if (quest.questLogEntryCompleted) {
            if (this.hasCharacter('qingyi_guest')) {
                this.guideActions.appendChild(
                    this.createGuideAction('听青衣客道别', { talk: 'qingyi_guest' }, 'talk')
                );
            } else if (this.hasExit('South')) {
                this.guideActions.appendChild(this.createGuideAction('回客栈大堂', { go: 'South' }, 'move'));
            }
            return;
        }

        switch (quest.questLogEntryStage) {
            case 'accepted':
                if (this.hasCharacter('qingyi_guest')) {
                    this.guideActions.appendChild(
                        this.createGuideAction('把冷酒交给青衣客', { talk: 'qingyi_guest' }, 'talk')
                    );
                } else if (this.hasExit('North')) {
                    this.guideActions.appendChild(this.createGuideAction('进客栈大堂', { go: 'North' }, 'move'));
                }
                break;
            case 'witness':
                if (this.hasCharacter('paper_umbrella_killer')) {
                    this.guideActions.appendChild(
                        this.createGuideAction('直面纸伞客', { talk: 'paper_umbrella_killer' }, 'talk')
                    );
                } else if (this.hasExit('NorthEast')) {
                    this.guideActions.appendChild(this.createGuideAction('去后院天井', { go: 'NorthEast' }, 'move'));
                }
                break;
            case 'duel':
                if (this.hasCharacter('paper_umbrella_killer')) {
                    this.guideActions.appendChild(
                        this.createGuideAction('迎战纸伞客', { attack: 'paper_umbrella_killer' }, 'combat')
                    );
                }
                break;
            default:
                if (this.currentRoom.exits.length > 0) {
                    this.guideActions.appendChild(this.createGuideNote(this.t('guide.check_exits')));
                }
        }
    }

    createGuideAction(label, action, kind = 'default') {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `guide-action guide-action-${kind}`;
        button.textContent = label;
        button.disabled = !this.connected;
        button.addEventListener('click', () => this.sendAction(action));
        return button;
    }

    createGuideNote(label) {
        const note = document.createElement('span');
        note.className = 'guide-note';
        note.textContent = label;
        return note;
    }

    hasCharacter(charId) {
        return this.currentRoom.characters.some(char => char.id === charId);
    }

    hasExit(direction) {
        const normalized = this.normalizeDirection(direction);
        return this.currentRoom.exits.some(exit => this.getExitDirection(exit) === normalized);
    }

    getReadySkill() {
        const { qi, ap } = this.playerStats;
        return this.currentSkills.find(skill => {
            const cooldown = this.skillCooldowns[skill.skillSummaryId] || 0;
            const requires = skill.skillSummaryReqStatus || [];
            const hasRequirements = requires.every(req => this.activeEffects.some(effect => effect.id === req));
            return hasRequirements
                && cooldown <= 0
                && qi >= skill.skillSummaryCost
                && ap >= skill.skillSummaryApReq;
        });
    }

    sendAction(action) {
        if (this.connected && this.ws) {
            if (action.choose) {
                this.lastStoryChoices = [];
                this.updateStoryGuide();
            }
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
        const paramPromptKey = btn.dataset.paramPromptKey;

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

        if (paramPromptKey) {
            // Button needs user input
            const userInput = prompt(this.t(paramPromptKey));
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
            const requires = (card.dataset.requires || '').split(',').filter(Boolean);
            const missing = requires.filter(req => !this.activeEffects.some(e => e.id === req));
            const cost = Number(card.dataset.cost || 0);
            const apReq = Number(card.dataset.apReq || 0);
            if (missing.length > 0) {
                this.displayMessage(`Requires ${missing.join(', ')} to be active`, 'error');
            } else if (this.playerStats.qi < cost) {
                this.displayMessage(`Need ${cost} Qi`, 'error');
            } else if (this.playerStats.ap < apReq) {
                this.displayMessage(`Need ${apReq} AP`, 'error');
            }
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
            this.connectionStatus.textContent = this.t('status.connected');
            this.connectionStatus.className = 'status-connected';
        } else {
            this.connectionStatus.textContent = this.t('status.disconnected');
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
