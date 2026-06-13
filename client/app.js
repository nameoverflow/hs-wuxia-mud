// WebSocket MUD Client
class MUDClient {
    constructor() {
        this.i18n = window.MUD_I18N || {};
        this.locale = this.getInitialLocale();
        this.testEntry = this.getTestEntryConfig();
        this.ws = null;
        this.username = null;
        this.connected = false;
        this.inBattle = false;
        this.currentEnemy = null;
        this.currentEnemyId = null;
        this.battlePlayerIdentity = { id: null, name: null };
        this.battleEnemyIdentity = { id: null, name: null };
        this.playerStatusRaw = null;
        this.playerStats = {
            hp: 100, maxHp: 100,
            qi: 100, maxQi: 100,
            ap: 0
        };
        this.apDisplayValue = 0;
        this.apTargetValue = 0;
        this.apAnimationFrame = null;
        this.lastApSnapshotAt = 0;
        this.enemyApDisplayValue = 0;
        this.enemyApTargetValue = 0;
        this.enemyApAnimationFrame = null;
        this.lastEnemyApSnapshotAt = 0;
        this.activeSkillCooldowns = {};
        this.activeEffects = [];
        this.currentActiveSkills = [];
        this.artsState = [];
        this.currentRoom = { name: null, desc: null, characters: [], exits: [] };
        this.questEntries = [];
        this.inventoryState = { money: 0, items: [] };
        this.activeNpc = null;
        this.currentRoomKey = null;
        this.pendingMapMove = null;
        this.mapMoveTimeout = null;
        this.mapAnimationCleanup = null;
        this.battleAnimationTimer = null;
        this.combatEventQueue = [];
        this.combatEventPlaying = false;
        this.combatEventTimer = null;
        this.battleExitPending = false;
        this.preBattleFilter = 'all';
        this.soundEnabled = false;
        this.audioContext = null;

        // DOM Elements
        this.mainContent = document.querySelector('.main-content');
        this.gamePanel = document.querySelector('.game-panel');
        this.messageDisplay = document.getElementById('message-display');
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
        this.battlePlayerHpText = document.getElementById('battle-player-hp-text');
        this.battleEnemyHpText = document.getElementById('battle-enemy-hp-text');
        this.battlePlayerQi = document.getElementById('battle-player-qi');
        this.battleEnemyQi = document.getElementById('battle-enemy-qi');
        this.battlePlayerQiText = document.getElementById('battle-player-qi-text');
        this.battleEnemyQiText = document.getElementById('battle-enemy-qi-text');
        this.battlePlayerAp = document.getElementById('battle-player-ap');
        this.battleEnemyAp = document.getElementById('battle-enemy-ap');
        this.battlePlayerApText = document.getElementById('battle-player-ap-text');
        this.battleEnemyApText = document.getElementById('battle-enemy-ap-text');
        this.battleStage = document.getElementById('battle-stage');
        this.battlePlayerFigure = document.getElementById('battle-player-figure');
        this.battleEnemyFigure = document.getElementById('battle-enemy-figure');
        this.battleStrike = document.getElementById('battle-strike');
        this.battleFloatDamage = document.getElementById('battle-float-damage');
        this.battleEffectLabel = document.getElementById('battle-effect-label');
        this.setBattleCombatantAvatars(null, null);

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
        this.initializeTestEntry();
    }

    getTestEntryConfig() {
        const params = new URLSearchParams(window.location.search);
        const enabled = params.get('test') === '1' || params.get('dev') === '1';
        if (!enabled) return { enabled: false };
        return {
            enabled: true,
            username: params.get('user') || 'tester',
            reset: params.get('reset') !== '0'
        };
    }

    initializeTestEntry() {
        if (!this.testEntry.enabled) return;
        this.usernameInput.value = this.testEntry.username;
        document.body.classList.add('test-entry-active');
        this.displayMessage(this.t('connection.test_entry', { user: this.testEntry.username }), 'system');
        window.setTimeout(() => {
            this.connect({
                username: this.testEntry.username,
                password: this.testEntry.reset ? '__dev_reset' : ''
            });
        }, 0);
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
        if (this.inBattle) {
            this.renderActiveSkillPanel(this.currentActiveSkills);
        } else {
            this.renderArtsPanel();
        }
        if (this.activeNpc) {
            this.renderNpcModal(this.activeNpc);
        }
        this.updateActiveSkillAvailability();
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

        // Active skill cards
        document.querySelectorAll('.active-skill-card').forEach(card => {
            card.addEventListener('click', () => this.handleActiveSkillClick(card));
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
                this.setMessageFilter(btn.dataset.filter || 'all');
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
            case 'active-skill':
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
                const isCombat =
                    msg.classList.contains('message-combat')
                    || msg.classList.contains('message-active-skill')
                    || msg.classList.contains('message-battle-error');
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

    setMessageFilter(filter) {
        this.currentFilter = filter || 'all';
        document.querySelectorAll('.filter-btn').forEach(button => {
            button.classList.toggle('active', button.dataset.filter === this.currentFilter);
        });
        this.applyMessageFilter();
    }

    connect(options = {}) {
        if (this.connected) return;
        const username = String(options.username || this.usernameInput.value || '').trim();
        if (!username) {
            this.displayMessage(this.t('error.username_required'), 'error');
            return;
        }
        const password = options.password || "";

        this.username = username;
        this.usernameInput.value = username;
        this.displayMessage(this.t('connection.connecting', { user: username }), 'system');

        try {
            this.ws = new WebSocket('ws://127.0.0.1:9160');

            this.ws.onopen = () => {
                this.connected = true;
                this.updateConnectionStatus(true);
                this.displayMessage(this.t('connection.connected'), 'system');

                // Send login event as JSON with tag
                const loginEvent = JSON.stringify({
                    tag: "Login",
                    username: username,
                    password
                });
                this.ws.send(loginEvent);

                this.loginBtn.disabled = true;
                this.usernameInput.disabled = true;

                // Update player name
                this.playerName.textContent = username;
                this.battlePlayerName.textContent = username;
                this.battlePlayerIdentity = {
                    id: `player$char$${username}`,
                    name: username
                };

                // Request initial view
                setTimeout(() => {
                    this.sendAction({ other: "view" });
                    this.sendAction({ other: "arts" });
                }, 500);
            };

            this.ws.onmessage = (event) => {
                this.handleMessage(event.data);
            };

            this.ws.onerror = (error) => {
                this.displayMessage(this.t('connection.error'), 'error');
                console.error('WebSocket error:', error);
            };

            this.ws.onclose = () => {
                this.connected = false;
                this.updateConnectionStatus(false);
                this.displayMessage(this.t('connection.disconnected'), 'system');
                this.updateStoryGuide();

                this.loginBtn.disabled = false;
                this.usernameInput.disabled = false;
            };

        } catch (error) {
            this.displayMessage(this.t('connection.failed', { detail: error.message }), 'error');
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
                case 'ActiveSkillMsg':
                    this.handleActiveSkillMsg(contents);
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
                case 'ActiveSkillFailureMsg':
                    this.handleActiveSkillFailureMsg(contents);
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
                case 'ArtsMsg':
                    this.handleArtsMsg(contents);
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
                case 'SystemMsg':
                    this.handleSystemMsg(contents);
                    break;
                case 'ErrorMsg':
                    this.handleErrorMsg(contents);
                    break;
                default:
                    // Fallback: display as JSON string
                    this.displayMessage(JSON.stringify(message, null, 2), 'system');
            }
        }
    }

    handleMoveMsg(contents) {
        this.displayMessage(this.t('message.move', { room: contents }), 'move');
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

        const normalizedExits = this.normalizeExitSummaries(exits || []);
        this.currentRoomKey = this.resolveCurrentRoomKey(roomName);
        this.pendingMapMove = null;
        if (this.mapMoveTimeout) {
            clearTimeout(this.mapMoveTimeout);
            this.mapMoveTimeout = null;
        }

        this.currentRoom = {
            name: roomName,
            desc: roomDesc,
            characters: parsedChars,
            exits: normalizedExits
        };

        this.renderRoom();
        this.updateStoryGuide();
        this.displayViewMessage();
    }

    renderRoom() {
        if (!this.roomName || !this.roomDesc || !this.roomCharacters || !this.roomExits) return;
        const { name, desc, characters, exits } = this.currentRoom;

        if (this.activeNpc && this.activeNpc.id && !characters.some(char => char.id === this.activeNpc.id)) {
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

    findCharacterByName(name) {
        return (this.currentRoom.characters || []).find(char => char.name === name) || null;
    }

    createNpcActionButton(label, kind, onClick, disabledReason = null) {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `npc-action npc-action-${kind}`;
        button.textContent = label;

        if (disabledReason) {
            button.disabled = true;
            button.title = disabledReason;
            button.setAttribute('aria-label', this.t('ui.label_reason', { label, reason: disabledReason }));
        } else if (onClick) {
            button.addEventListener('click', onClick);
        }

        return button;
    }

    renderRoomMap(exits) {
        if (!this.roomMap) return;

        const previousRects = this.captureMapNodeRects();
        this.roomMap.textContent = '';
        this.roomMap.classList.remove('map-moving');
        const currentPoint = { x: 50, y: 56 };
        const currentPosition = this.inferCurrentMapPosition(exits);
        const exitPoints = exits
            .map(exit => {
                const direction = this.getExitDirection(exit);
                const label = this.getExitLabel(exit);
                if (!direction) return null;

                const targetPosition = this.getExitPosition(exit);
                const fallbackVector = this.directionVector(direction);
                const dx = targetPosition ? targetPosition.x - currentPosition.x : fallbackVector.x;
                const dy = targetPosition ? targetPosition.y - currentPosition.y : fallbackVector.y;
                const vector = (dx === 0 && dy === 0) ? fallbackVector : { x: dx, y: dy };

                return {
                    direction,
                    label,
                    roomKey: this.getExitRoomKey(exit, label),
                    x: this.clamp(50 + vector.x * 28, 14, 86),
                    y: this.clamp(56 - vector.y * 30, 18, 88)
                };
            })
            .filter(Boolean);

        const links = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        links.classList.add('map-links-svg');
        links.setAttribute('viewBox', '0 0 100 100');
        links.setAttribute('preserveAspectRatio', 'none');
        exitPoints.forEach(point => {
            const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
            line.setAttribute('x1', String(currentPoint.x));
            line.setAttribute('y1', String(currentPoint.y));
            line.setAttribute('x2', String(point.x));
            line.setAttribute('y2', String(point.y));
            links.appendChild(line);
        });
        this.roomMap.appendChild(links);

        const currentNode = document.createElement('div');
        currentNode.className = 'map-node map-node-current';
        currentNode.dataset.roomKey = this.currentRoomKey || this.getRoomNameKey(this.currentRoom.name);
        currentNode.textContent = this.currentRoom.name || this.t('map.current_position');
        currentNode.style.left = `${currentPoint.x}%`;
        currentNode.style.top = `${currentPoint.y}%`;
        this.roomMap.appendChild(currentNode);

        if (exitPoints.length === 0) {
            const empty = document.createElement('div');
            empty.className = 'map-empty';
            empty.textContent = this.t('map.no_exits');
            this.roomMap.appendChild(empty);
            return;
        }

        exitPoints.forEach(point => {
            const node = document.createElement('button');
            node.type = 'button';
            node.className = 'map-node map-node-exit';
            node.dataset.direction = point.direction;
            node.dataset.roomKey = point.roomKey;
            node.textContent = point.label;
            node.style.left = `${point.x}%`;
            node.style.top = `${point.y}%`;
            node.title = this.t('map.move_to', { value: point.label });
            node.setAttribute('aria-label', this.t('map.move_to', { value: point.label }));
            node.disabled = !this.connected;
            node.addEventListener('click', () => this.goDirection(point.direction));
            this.roomMap.appendChild(node);
        });

        this.animateMapTransition(previousRects);
    }

    captureMapNodeRects() {
        const rects = new Map();
        if (!this.roomMap) return rects;

        this.roomMap.querySelectorAll('.map-node[data-room-key]').forEach(node => {
            const key = node.dataset.roomKey;
            if (!key) return;
            rects.set(key, node.getBoundingClientRect());
        });
        return rects;
    }

    animateMapTransition(previousRects) {
        if (!this.roomMap || previousRects.size === 0) return;

        const animatedNodes = [];
        this.roomMap.querySelectorAll('.map-node[data-room-key]').forEach(node => {
            const previous = previousRects.get(node.dataset.roomKey);
            if (!previous) return;

            const next = node.getBoundingClientRect();
            const dx = previous.left - next.left;
            const dy = previous.top - next.top;
            if (Math.abs(dx) < 1 && Math.abs(dy) < 1) return;

            node.classList.add('map-node-animating');
            node.style.setProperty('--move-x', `${dx}px`);
            node.style.setProperty('--move-y', `${dy}px`);
            animatedNodes.push(node);
        });

        if (animatedNodes.length === 0) return;
        void this.roomMap.offsetWidth;

        animatedNodes.forEach(node => {
            node.classList.remove('map-node-animating');
            node.classList.add('map-node-settling');
            node.style.setProperty('--move-x', '0px');
            node.style.setProperty('--move-y', '0px');
        });

        if (this.mapAnimationCleanup) {
            clearTimeout(this.mapAnimationCleanup);
        }
        this.mapAnimationCleanup = setTimeout(() => {
            animatedNodes.forEach(node => {
                node.classList.remove('map-node-settling');
                node.style.removeProperty('--move-x');
                node.style.removeProperty('--move-y');
            });
            this.mapAnimationCleanup = null;
        }, 240);
    }

    resolveCurrentRoomKey(roomName) {
        const pending = this.pendingMapMove;
        if (pending && (!pending.roomName || pending.roomName === roomName)) {
            return pending.roomKey;
        }
        if (this.currentRoom.name === roomName && this.currentRoomKey) {
            return this.currentRoomKey;
        }
        return this.getRoomNameKey(roomName);
    }

    getExitRoomKey(exit, label) {
        if (exit && typeof exit === 'object' && exit.roomId) {
            return `id:${exit.roomId}`;
        }
        if (exit && typeof exit === 'object' && exit.roomName) {
            return this.getRoomNameKey(exit.roomName);
        }
        return this.getRoomNameKey(label || this.getExitDirection(exit));
    }

    getRoomNameKey(name) {
        return `name:${name || 'current'}`;
    }

    inferCurrentMapPosition(exits) {
        const candidates = exits
            .map(exit => {
                const position = this.getExitPosition(exit);
                const vector = this.directionVector(this.getExitDirection(exit));
                if (!position) return null;
                return {
                    x: position.x - vector.x,
                    y: position.y - vector.y
                };
            })
            .filter(Boolean);

        if (candidates.length === 0) return { x: 0, y: 0 };

        const counts = new Map();
        candidates.forEach(candidate => {
            const key = `${candidate.x},${candidate.y}`;
            counts.set(key, (counts.get(key) || 0) + 1);
        });

        const [bestKey] = [...counts.entries()].sort((a, b) => b[1] - a[1])[0];
        const [x, y] = bestKey.split(',').map(Number);
        return { x, y };
    }

    getExitPosition(exit) {
        const raw = exit && exit.position;
        if (Array.isArray(raw) && raw.length >= 2) {
            return { x: Number(raw[0]), y: Number(raw[1]) };
        }
        if (raw && typeof raw === 'object' && Number.isFinite(Number(raw.x)) && Number.isFinite(Number(raw.y))) {
            return { x: Number(raw.x), y: Number(raw.y) };
        }
        return null;
    }

    directionVector(direction) {
        const vectors = {
            North: { x: 0, y: 1 },
            South: { x: 0, y: -1 },
            East: { x: 1, y: 0 },
            West: { x: -1, y: 0 },
            NorthEast: { x: 1, y: 1 },
            NorthWest: { x: -1, y: 1 },
            SouthEast: { x: 1, y: -1 },
            SouthWest: { x: -1, y: -1 }
        };
        return vectors[direction] || { x: 0, y: 0 };
    }

    clamp(value, min, max) {
        return Math.max(min, Math.min(max, value));
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
        const defenderCharacter = this.findCharacterByName(defender);
        const msg = this.t('message.attack', { attacker, defender });
        this.clearCombatEventQueue();
        this.displayMessage(msg, 'combat');
        this.addCombatLog(msg);
        this.playSound('attack');

        // Enter battle mode
        this.setBattleMode(true, defender, defenderCharacter && defenderCharacter.id);
    }

    handleCombatNormalMsg(contents) {
        const [attacker, defender, combatMessage, damage] = contents;
        const actionText = this.formatCombatMessage(combatMessage);
        const msg = this.t('message.combat.damage', {
            attacker,
            defender,
            action: actionText,
            damage
        });
        this.enqueueCombatEvent({
            type: 'motion',
            message: msg,
            messageType: 'combat',
            sound: 'damage',
            attacker,
            defender,
            damage,
            label: this.t('battle.damage_tag'),
            duration: 460,
            motionOptions: {
                attackerMotion: 'attack',
                defenderMotion: this.classifyDefenseMotion(actionText, damage)
            }
        });
    }

    handleCombatSettlementMsg(contents) {
        const [player, enemy, won] = contents;
        const msg = this.t(won ? 'message.combat.victory' : 'message.combat.defeat', { player, enemy });
        this.enqueueCombatEvent({
            type: 'settlement',
            message: msg,
            messageType: 'combat',
            sound: won ? 'victory' : 'defeat',
            won,
            duration: 520
        });
    }

    handleActiveSkillMsg(contents) {
        const [caster, target, activeSkillMessage] = contents;
        const actionText = this.formatCombatMessage(activeSkillMessage);
        const msg = this.t('message.active_skill', {
            caster,
            target,
            action: actionText
        });
        const motion = this.classifyActiveSkillMotion(actionText, caster, target);
        this.enqueueCombatEvent({
            type: 'motion',
            message: msg,
            messageType: 'active-skill',
            sound: 'active-skill',
            attacker: caster,
            defender: target,
            damage: 0,
            label: this.t('battle.skill_tag'),
            duration: 460,
            motionOptions: {
                attackerMotion: motion,
                defenderMotion: caster === target ? null : undefined
            }
        });
    }

    handleActiveSkillFailureMsg(contents) {
        const msg = this.formatActiveSkillFailure(contents);
        this.displayMessage(msg, 'battle-error');
        this.addCombatLog(this.t('message.active_skill_failed', { reason: msg }));
    }

    handleDialogueMsg(contents) {
        const [charName, dialogue] = contents;
        this.displayMessage(this.t('message.dialogue', { speaker: charName, text: dialogue }), 'dialogue');
    }

    handleStoryMsg(contents) {
        const [speaker, text] = contents;
        const messageDiv = this.createStoryHistoryMessage(speaker, text);
        this.messageDisplay.appendChild(messageDiv);
        this.messageDisplay.scrollTop = this.messageDisplay.scrollHeight;
        this.applyMessageFilter();
        this.updateStoryGuide();
    }

    createStoryHistoryMessage(speaker, text) {
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

        return messageDiv;
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
        if (this.connected && rewards.some(reward => reward.rewardSummaryKind === 'martial_art')) {
            this.sendAction({ other: 'arts' });
        }
    }

    handleArtsMsg(arts) {
        this.artsState = arts || [];
        if (!this.inBattle) {
            this.renderArtsPanel();
        }
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
        if (status === 'in_battle' && !this.inBattle) {
            this.setBattleMode(true);
        } else if (status !== 'in_battle' && this.inBattle) {
            if (this.hasPendingCombatEvents()) {
                this.battleExitPending = true;
            } else {
                this.setBattleMode(false);
            }
        }
        this.updateStoryGuide();
    }

    handleBattleStateMsg(snapshot) {
        const player = snapshot.battleSnapshotPlayer;
        const enemy = snapshot.battleSnapshotEnemy;
        const cooldowns = snapshot.battleSnapshotActiveSkillCooldowns || [];
        const activeSkills = snapshot.battleSnapshotActiveSkills || [];

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

        this.activeSkillCooldowns = {};
        cooldowns.forEach(cd => {
            this.activeSkillCooldowns[cd.activeSkillCooldownSummaryActiveSkillId] = cd.activeSkillCooldownSummaryRemaining;
        });

        this.currentActiveSkills = activeSkills;
        this.renderActiveSkillPanel(activeSkills);

        this.battlePlayerIdentity = {
            id: player.combatantSnapshotId || this.battlePlayerIdentity.id,
            name: player.combatantSnapshotName || this.battlePlayerIdentity.name
        };
        this.battleEnemyIdentity = {
            id: enemy.combatantSnapshotId || this.battleEnemyIdentity.id,
            name: enemy.combatantSnapshotName || this.battleEnemyIdentity.name
        };
        if (this.battlePlayerName && this.battlePlayerIdentity.name) {
            this.battlePlayerName.textContent = this.battlePlayerIdentity.name;
        }

        this.setBattleMode(true, enemy.combatantSnapshotName, enemy.combatantSnapshotId);
        this.updateResourceBars();
        this.setBattleCombatantAvatars(player.combatantSnapshotId, enemy.combatantSnapshotId);
        this.updateBattleEnemy(enemy);
        this.renderActiveEffects();
        this.updateActiveSkillAvailability();
        this.updateStoryGuide();
    }

    handleSayMsg(contents) {
        const [speaker, message] = contents;
        this.displayMessage(this.t('message.say', { speaker, text: message }), 'say');
    }

    handleUseItemMsg(contents) {
        const [, itemDesc] = contents;
        this.displayMessage(itemDesc, 'system');
    }

    handleSystemMsg(contents) {
        const key = contents && contents.systemMessageKey;
        const params = this.normalizeMessageParams(contents && contents.systemMessageParams);
        const i18nKey = `system.${key}`;
        const msg = this.hasTranslation(i18nKey)
            ? this.t(i18nKey, params)
            : this.t('system.unknown', { code: key || 'unknown' });
        this.displayMessage(msg, 'system');
    }

    handleErrorMsg(contents) {
        const code = contents && contents.errorSummaryCode;
        const params = this.localizeErrorParams(this.normalizeMessageParams(contents && contents.errorSummaryParams));
        const i18nKey = `error.${code}`;
        const msg = this.hasTranslation(i18nKey)
            ? this.t(i18nKey, params)
            : this.t('error.unknown', { code: code || 'unknown', detail: params.detail || '' });
        this.displayMessage(msg, 'error');
    }

    localizeErrorParams(params) {
        const localized = { ...params };
        if (localized.action) {
            const key = `action.${localized.action}`;
            localized.action = this.hasTranslation(key) ? this.t(key) : localized.action;
        }
        if (localized.direction) {
            localized.direction = this.formatDirection(localized.direction);
        }
        return localized;
    }

    normalizeMessageParams(params) {
        if (!params || typeof params !== 'object') return {};
        return Object.fromEntries(
            Object.entries(params).map(([key, value]) => [key, value === null || value === undefined ? '' : value])
        );
    }

    formatCombatMessage(combatMessage) {
        if (!combatMessage || typeof combatMessage !== 'object') return '';
        if (combatMessage.kind === 'script') {
            return combatMessage.text || '';
        }
        if (combatMessage.kind === 'effect_tick') {
            const effect = combatMessage.effectName || combatMessage.effectId || '';
            const amount = combatMessage.amount || 0;
            const key = `message.combat.effect.${combatMessage.effectKind}`;
            return this.hasTranslation(key)
                ? this.t(key, { effect, amount })
                : this.t('message.combat.effect.unknown', { effect, amount });
        }
        return '';
    }

    formatActiveSkillFailure(reason) {
        if (!reason || typeof reason !== 'object') {
            return this.t('active_skill.failure.unknown');
        }

        switch (reason.reason) {
            case 'need_ap':
                return this.t('active_skill.failure.need_ap', {
                    required: reason.required,
                    current: reason.current
                });
            case 'need_qi':
                return this.t('active_skill.failure.need_qi', {
                    required: reason.required,
                    current: reason.current
                });
            case 'cooldown':
                return this.t('active_skill.failure.cooldown', {
                    seconds: reason.remaining
                });
            case 'missing_status':
                return this.t('active_skill.failure.missing_status', {
                    value: (reason.statuses || []).map(status => this.formatEffectName(status)).join(', ')
                });
            case 'unavailable':
                return this.t('active_skill.failure.unavailable', {
                    activeSkill: reason.activeSkillId || ''
                });
            default:
                return this.t('active_skill.failure.unknown');
        }
    }

    updateResourceBars() {
        const { hp, maxHp, qi, maxQi, ap } = this.playerStats;

        // Health bar
        const hpPercent = this.percent(hp, maxHp);
        this.hpBarFill.style.width = `${hpPercent}%`;
        this.hpText.textContent = `${hp}/${maxHp}`;

        // Inner power bar
        const qiPercent = this.percent(qi, maxQi);
        this.qiBarFill.style.width = `${qiPercent}%`;
        this.qiText.textContent = `${qi}/${maxQi}`;

        this.animatePlayerApTo(ap);

        // Update battle health bar
        this.battlePlayerHp.style.width = `${hpPercent}%`;
        if (this.battlePlayerHpText) {
            this.battlePlayerHpText.textContent = `${hp}/${maxHp}`;
        }
        if (this.battlePlayerQi) {
            this.battlePlayerQi.style.width = `${qiPercent}%`;
        }
        if (this.battlePlayerQiText) {
            this.battlePlayerQiText.textContent = `${qi}/${maxQi}`;
        }

        // Update active skill availability based on resources
        this.updateActiveSkillAvailability();
    }

    animatePlayerApTo(target) {
        this.animateBattleApTo({
            target,
            getDisplay: () => this.apDisplayValue,
            setDisplay: value => { this.apDisplayValue = value; },
            getTarget: () => this.apTargetValue,
            setTarget: value => { this.apTargetValue = value; },
            getFrame: () => this.apAnimationFrame,
            setFrame: value => { this.apAnimationFrame = value; },
            getLastSnapshotAt: () => this.lastApSnapshotAt,
            setLastSnapshotAt: value => { this.lastApSnapshotAt = value; },
            render: value => this.renderPlayerApValue(value)
        });
    }

    animateEnemyApTo(target) {
        this.animateBattleApTo({
            target,
            getDisplay: () => this.enemyApDisplayValue,
            setDisplay: value => { this.enemyApDisplayValue = value; },
            getTarget: () => this.enemyApTargetValue,
            setTarget: value => { this.enemyApTargetValue = value; },
            getFrame: () => this.enemyApAnimationFrame,
            setFrame: value => { this.enemyApAnimationFrame = value; },
            getLastSnapshotAt: () => this.lastEnemyApSnapshotAt,
            setLastSnapshotAt: value => { this.lastEnemyApSnapshotAt = value; },
            render: value => this.renderBattleAp(this.battleEnemyAp, this.battleEnemyApText, value)
        });
    }

    animateBattleApTo(config) {
        const targetValue = Number.isFinite(Number(config.target)) ? Number(config.target) : 0;
        const currentDisplay = config.getDisplay();
        const currentValue = Number.isFinite(currentDisplay) ? currentDisplay : targetValue;
        const sameTarget = Math.abs(targetValue - config.getTarget()) < 0.01;

        if (sameTarget && config.getFrame() !== null) {
            return;
        }

        if (sameTarget && Math.abs(currentValue - targetValue) < 0.01) {
            config.setDisplay(targetValue);
            config.render(targetValue);
            return;
        }

        if (config.getFrame() !== null) {
            window.cancelAnimationFrame(config.getFrame());
            config.setFrame(null);
        }

        const now = window.performance.now();
        const lastSnapshotAt = config.getLastSnapshotAt();
        const snapshotGap = lastSnapshotAt > 0 ? now - lastSnapshotAt : 650;
        config.setLastSnapshotAt(now);
        config.setTarget(targetValue);

        const decreasing = targetValue < currentValue;
        const duration = decreasing ? 140 : Math.max(240, Math.min(950, snapshotGap));
        const startTime = now;
        config.setDisplay(currentValue);
        config.render(currentValue);

        const step = (frameTime) => {
            const progress = Math.max(0, Math.min(1, (frameTime - startTime) / duration));
            const easedProgress = decreasing ? 1 - Math.pow(1 - progress, 3) : progress;
            const nextValue = currentValue + (targetValue - currentValue) * easedProgress;
            config.setDisplay(nextValue);
            config.render(nextValue);

            if (progress < 1) {
                config.setFrame(window.requestAnimationFrame(step));
                return;
            }

            config.setDisplay(targetValue);
            config.render(targetValue);
            config.setFrame(null);
        };

        config.setFrame(window.requestAnimationFrame(step));
    }

    renderPlayerApValue(value) {
        const displayValue = Number.isFinite(value) ? Math.max(0, value) : 0;
        this.apDisplayValue = displayValue;
        this.apBarFill.style.width = `${Math.min(displayValue, 100)}%`;
        this.apText.textContent = String(Math.round(displayValue));
        this.renderBattleAp(this.battlePlayerAp, this.battlePlayerApText, displayValue);
    }

    renderBattleAp(bar, text, value) {
        const displayValue = Number.isFinite(Number(value)) ? Math.max(0, Number(value)) : 0;
        if (bar) {
            bar.style.width = `${Math.min(displayValue, 100)}%`;
        }
        if (text) {
            text.textContent = `${this.t('resource.ap')} ${Math.round(displayValue)}`;
        }
    }

    updateActiveSkillAvailability() {
        const { qi, ap } = this.playerStats;

        document.querySelectorAll('.active-skill-card').forEach(card => {
            const activeSkillId = card.dataset.activeSkillId;
            const requires = (card.dataset.requires || '').split(',').filter(Boolean);
            const cost = Number(card.dataset.cost || 0);
            const apReq = Number(card.dataset.apReq || 0);

            // Check if the active skill is locked due to missing buff/resources.
            const hasRequirements = requires.every(req => this.activeEffects.some(e => e.id === req));
            const hasResources = qi >= cost && ap >= apReq;
            card.classList.toggle('active-skill-locked', !hasRequirements || !hasResources);

            // Check cooldown
            const remaining = this.activeSkillCooldowns[activeSkillId] || 0;
            card.classList.toggle('on-cooldown', remaining > 0);
            card.classList.toggle('active-skill-ready', hasRequirements && hasResources && remaining <= 0);
            const cooldownBar = card.querySelector('.active-skill-cooldown');
            if (cooldownBar) {
                const cooldown = Number(card.dataset.cooldown || remaining || 1);
                cooldownBar.style.width = remaining > 0 ? `${this.percent(remaining, cooldown)}%` : '0%';
            }

            const status = card.querySelector('.active-skill-status');
            if (status) {
                if (remaining > 0) {
                    status.textContent = this.t('active_skill.status.cd', { seconds: Math.ceil(remaining) });
                } else if (!hasRequirements) {
                    status.textContent = this.t('active_skill.status.need', { value: this.getRequirementLabels(card, requires).join(', ') });
                } else if (qi < cost) {
                    status.textContent = this.t('active_skill.status.qi', { current: qi, required: cost });
                } else if (ap < apReq) {
                    status.textContent = this.t('active_skill.status.ap', { current: Math.round(ap), required: apReq });
                } else {
                    status.textContent = this.t('active_skill.status.ready');
                }
            }
        });
    }

    updateBattleEnemy(enemy) {
        const hpPercent = this.percent(enemy.combatantSnapshotHp, enemy.combatantSnapshotMaxHp);
        const qiPercent = this.percent(enemy.combatantSnapshotQi, enemy.combatantSnapshotMaxQi);
        this.battleEnemyName.textContent = enemy.combatantSnapshotName;
        this.battleEnemyHp.style.width = `${hpPercent}%`;
        if (this.battleEnemyHpText) {
            this.battleEnemyHpText.textContent = `${enemy.combatantSnapshotHp}/${enemy.combatantSnapshotMaxHp}`;
        }
        if (this.battleEnemyQi) {
            this.battleEnemyQi.style.width = `${qiPercent}%`;
        }
        if (this.battleEnemyQiText) {
            this.battleEnemyQiText.textContent = `${enemy.combatantSnapshotQi}/${enemy.combatantSnapshotMaxQi}`;
        }
        this.animateEnemyApTo(enemy.combatantSnapshotAp);
        this.battleEnemyFigure?.classList.toggle('fighter-defeated', enemy.combatantSnapshotHp <= 0);
    }

    enqueueCombatEvent(event) {
        if (!event) return;
        this.combatEventQueue.push(event);
        this.playNextCombatEvent();
    }

    hasPendingCombatEvents() {
        return this.combatEventPlaying || this.combatEventQueue.length > 0;
    }

    playNextCombatEvent() {
        if (this.combatEventPlaying) return;
        const event = this.combatEventQueue.shift();
        if (!event) {
            if (this.battleExitPending && this.inBattle) {
                this.setBattleMode(false);
            }
            return;
        }

        this.combatEventPlaying = true;
        if (event.type === 'settlement') {
            this.playQueuedCombatSettlement(event);
        } else {
            this.playQueuedCombatMotion(event);
        }
    }

    playQueuedCombatMotion(event) {
        this.displayMessage(event.message, event.messageType || 'combat');
        this.addCombatLog(event.logMessage || event.message);
        if (event.sound) {
            this.playSound(event.sound);
        }

        const duration = event.duration || 460;
        this.playBattleMotion(event.attacker, event.defender, event.damage, event.label, {
            ...(event.motionOptions || {}),
            duration
        });
        this.finishCombatEventAfter(duration + 90);
    }

    playQueuedCombatSettlement(event) {
        this.displayMessage(event.message, event.messageType || 'combat');
        this.addCombatLog(event.logMessage || event.message);
        if (event.sound) {
            this.playSound(event.sound);
        }
        this.playBattleSettlement(event.won);
        this.battleExitPending = false;

        this.finishCombatEventAfter(event.duration || 520, () => {
            this.combatEventQueue = [];
            this.setBattleMode(false);
        });
    }

    finishCombatEventAfter(delay, afterFinish = null) {
        if (this.combatEventTimer) {
            clearTimeout(this.combatEventTimer);
        }
        this.combatEventTimer = setTimeout(() => {
            this.combatEventTimer = null;
            this.combatEventPlaying = false;
            if (afterFinish) {
                afterFinish();
            } else {
                this.playNextCombatEvent();
            }
        }, delay);
    }

    clearCombatEventQueue() {
        if (this.combatEventTimer) {
            clearTimeout(this.combatEventTimer);
            this.combatEventTimer = null;
        }
        this.combatEventQueue = [];
        this.combatEventPlaying = false;
        this.battleExitPending = false;
    }

    playBattleMotion(attacker, defender, damage = 0, label = null, options = {}) {
        if (!this.battleStage || !this.inBattle) return;

        if (this.battleAnimationTimer) {
            clearTimeout(this.battleAnimationTimer);
            this.battleAnimationTimer = null;
        }

        const resolvedAttackerSlot = options.attackerSlot || this.resolveCombatantSlot(attacker);
        const resolvedDefenderSlot = options.defenderSlot || this.resolveCombatantSlot(defender);
        const attackerSlot = resolvedAttackerSlot
            || (resolvedDefenderSlot === 'enemy' ? 'player' : resolvedDefenderSlot === 'player' ? 'enemy' : 'player');
        const defenderSlot = resolvedDefenderSlot
            || (attackerSlot === 'player' ? 'enemy' : 'player');
        const attackerFigure = this.battleFigureForSlot(attackerSlot);
        const defenderFigure = this.battleFigureForSlot(defenderSlot);
        const sideClass = attackerSlot === 'player' ? 'battle-action-player' : 'battle-action-enemy';
        const floatSideClass = defenderSlot === 'player' ? 'battle-float-player' : 'battle-float-enemy';
        const attackerMotion = options.attackerMotion || 'attack';
        const defenderMotion = options.defenderMotion === undefined
            ? (damage > 0 ? 'hit' : null)
            : options.defenderMotion;
        const showStrike = attackerMotion === 'attack' || attackerMotion === 'parry';

        this.clearBattleMotionClasses();
        this.battleStage.classList.add(sideClass);
        this.applyFighterMotion(attackerFigure, attackerMotion);
        if (defenderFigure && defenderFigure !== attackerFigure && defenderMotion) {
            this.applyFighterMotion(defenderFigure, defenderMotion);
        }
        if (showStrike) {
            this.battleStrike?.classList.add('battle-strike-visible', sideClass);
        }

        if (this.battleEffectLabel) {
            this.battleEffectLabel.textContent = label || this.t('battle.damage_tag');
            this.battleEffectLabel.classList.add('battle-effect-visible', floatSideClass);
        }

        if (this.battleFloatDamage) {
            this.battleFloatDamage.textContent = damage > 0 ? `-${damage}` : '';
            this.battleFloatDamage.classList.toggle('battle-float-visible', damage > 0);
            this.battleFloatDamage.classList.add(floatSideClass);
        }

        this.battleAnimationTimer = setTimeout(() => {
            this.clearBattleMotionClasses();
            this.battleAnimationTimer = null;
        }, options.duration || 460);
    }

    applyFighterMotion(figure, motion) {
        if (!figure || !motion) return;
        const className = {
            attack: 'fighter-attacking',
            hit: 'fighter-hit',
            dodge: 'fighter-dodge',
            parry: 'fighter-parry'
        }[motion];
        if (className) {
            figure.classList.add(className);
        }
    }

    classifyDefenseMotion(actionText, damage) {
        const text = String(actionText || '');
        if (/[挡架格拦封]/.test(text)) return 'parry';
        if (/[闪避躲让侧身折身]/.test(text) || damage <= 0) return 'dodge';
        return 'hit';
    }

    classifyActiveSkillMotion(actionText, caster, target) {
        const text = String(actionText || '');
        if (/[挡架格拦封]/.test(text)) return 'parry';
        if (/[闪避躲让侧身折身]/.test(text)) return 'dodge';
        if (caster === target || /气入|一掌按下|恢复|回复|疗/.test(text)) return 'parry';
        return 'attack';
    }

    playBattleSettlement(won) {
        const target = won ? this.battleEnemyFigure : this.battlePlayerFigure;
        target?.classList.add('fighter-defeated');
    }

    clearBattleMotionClasses() {
        this.battleStage?.classList.remove('battle-action-player', 'battle-action-enemy');
        this.battlePlayerFigure?.classList.remove('fighter-attacking', 'fighter-hit', 'fighter-dodge', 'fighter-parry');
        this.battleEnemyFigure?.classList.remove('fighter-attacking', 'fighter-hit', 'fighter-dodge', 'fighter-parry');
        this.battleStrike?.classList.remove('battle-strike-visible', 'battle-action-player', 'battle-action-enemy');
        this.battleEffectLabel?.classList.remove('battle-effect-visible', 'battle-float-player', 'battle-float-enemy');
        this.battleFloatDamage?.classList.remove('battle-float-visible', 'battle-float-player', 'battle-float-enemy');
    }

    battleFigureForSlot(slot) {
        return slot === 'player' ? this.battlePlayerFigure : this.battleEnemyFigure;
    }

    resolveCombatantSlot(raw) {
        const values = this.combatantValues(raw);
        if (this.matchesCombatant(values, this.battlePlayerIdentity, [
            this.username,
            this.battlePlayerName?.textContent
        ])) {
            return 'player';
        }
        if (this.matchesCombatant(values, this.battleEnemyIdentity, [
            this.currentEnemy,
            this.currentEnemyId,
            this.battleEnemyName?.textContent
        ])) {
            return 'enemy';
        }
        return null;
    }

    combatantValues(raw) {
        if (!raw) return [];
        if (typeof raw === 'object') {
            return [
                raw.id,
                raw.name,
                raw.combatantSnapshotId,
                raw.combatantSnapshotName,
                raw.combatantRefId,
                raw.combatantRefName
            ].filter(Boolean).map(value => String(value));
        }
        return [String(raw)];
    }

    matchesCombatant(values, identity, fallbacks = []) {
        const candidates = [
            identity && identity.id,
            identity && identity.name,
            ...fallbacks
        ].filter(Boolean).map(value => String(value));
        return values.some(value => candidates.includes(value));
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

    renderActiveSkillPanel(activeSkills) {
        const panel = document.getElementById('martial-arts-panel');
        panel.textContent = '';

        if (!activeSkills || activeSkills.length === 0) {
            const empty = document.createElement('span');
            empty.className = 'empty-list';
            empty.textContent = this.t('ui.none');
            panel.appendChild(empty);
            return;
        }

        activeSkills.forEach(activeSkill => {
            const card = document.createElement('div');
            card.className = 'active-skill-card';
            card.setAttribute('role', 'button');
            card.tabIndex = 0;
            if ((activeSkill.activeSkillSummaryDamage || 0) >= 100) {
                card.classList.add('active-skill-ultimate');
            }
            card.dataset.activeSkillId = activeSkill.activeSkillSummaryId;
            card.dataset.cost = String(activeSkill.activeSkillSummaryCost);
            card.dataset.apReq = String(activeSkill.activeSkillSummaryApReq);
            card.dataset.cooldown = String(activeSkill.activeSkillSummaryCooldown);
            card.dataset.requires = (activeSkill.activeSkillSummaryReqStatus || []).join(',');
            card.dataset.requiresLabel = (activeSkill.activeSkillSummaryReqStatusNames || []).join(',');
            card.title = activeSkill.activeSkillSummaryDesc || '';

            const header = document.createElement('div');
            header.className = 'active-skill-header';
            const name = document.createElement('span');
            name.className = 'active-skill-name';
            name.textContent = this.formatActiveSkillName(activeSkill.activeSkillSummaryId, activeSkill.activeSkillSummaryName);
            const cost = document.createElement('span');
            cost.className = 'active-skill-cost';
            cost.textContent = this.t('active_skill.cost.qi', { value: activeSkill.activeSkillSummaryCost });
            header.appendChild(name);
            header.appendChild(cost);

            const info = document.createElement('div');
            info.className = 'active-skill-info';
            const ap = document.createElement('span');
            ap.className = 'active-skill-ap';
            ap.textContent = this.t('active_skill.ap', { value: activeSkill.activeSkillSummaryApReq });
            info.appendChild(ap);
            if (activeSkill.activeSkillSummaryDamage !== null && activeSkill.activeSkillSummaryDamage !== undefined) {
                const damage = document.createElement('span');
                damage.className = 'active-skill-damage';
                damage.textContent = this.t('active_skill.damage', { value: activeSkill.activeSkillSummaryDamage });
                info.appendChild(damage);
            }
            if (activeSkill.activeSkillSummaryHeal !== null && activeSkill.activeSkillSummaryHeal !== undefined) {
                const heal = document.createElement('span');
                heal.className = 'active-skill-heal';
                heal.textContent = this.t('active_skill.heal', { value: activeSkill.activeSkillSummaryHeal });
                info.appendChild(heal);
            }

            card.appendChild(header);
            card.appendChild(info);

            if (activeSkill.activeSkillSummaryReqStatus && activeSkill.activeSkillSummaryReqStatus.length > 0) {
                const requirement = document.createElement('div');
                requirement.className = 'active-skill-requirement';
                requirement.textContent = this.t('active_skill.requires', {
                    value: (activeSkill.activeSkillSummaryReqStatusNames || activeSkill.activeSkillSummaryReqStatus.map(req => this.formatEffectName(req))).join(', ')
                });
                card.appendChild(requirement);
            }

            const status = document.createElement('div');
            status.className = 'active-skill-status';
            status.textContent = this.t('active_skill.status.waiting');
            card.appendChild(status);

            const cooldown = document.createElement('div');
            cooldown.className = 'active-skill-cooldown';
            card.appendChild(cooldown);

            card.addEventListener('click', () => this.handleActiveSkillClick(card));
            card.addEventListener('keydown', (event) => {
                if (event.key === 'Enter' || event.key === ' ') {
                    event.preventDefault();
                    this.handleActiveSkillClick(card);
                }
            });
            panel.appendChild(card);
        });
    }

    renderArtsPanel() {
        const panel = document.getElementById('martial-arts-panel');
        if (!panel || this.inBattle) return;

        panel.textContent = '';
        const arts = this.artsState || [];
        if (arts.length === 0) {
            const empty = document.createElement('span');
            empty.className = 'empty-list';
            empty.textContent = this.t('ui.none');
            panel.appendChild(empty);
            return;
        }

        arts.forEach(art => {
            const card = document.createElement('div');
            card.className = 'art-card';
            if (art.artSummaryIsFoundation) {
                card.classList.add('art-foundation');
            }

            const header = document.createElement('div');
            header.className = 'art-header';
            const name = document.createElement('span');
            name.className = 'art-name';
            name.textContent = art.artSummaryName || art.artSummaryId;
            const type = document.createElement('span');
            type.className = 'art-type';
            type.textContent = this.formatArtType(art.artSummaryType);
            header.appendChild(name);
            header.appendChild(type);
            card.appendChild(header);

            const level = document.createElement('div');
            level.className = 'art-line';
            level.textContent = this.t('art.level', {
                level: art.artSummaryLevel,
                max: art.artSummaryMaxLevel
            });
            card.appendChild(level);

            if (art.artSummaryFoundation) {
                const foundation = document.createElement('div');
                foundation.className = 'art-line';
                foundation.textContent = this.t('art.foundation', { value: art.artSummaryFoundation });
                card.appendChild(foundation);
            }

            const requirements = art.artSummaryRequirements || [];
            if (requirements.length > 0) {
                const req = document.createElement('div');
                req.className = 'art-line';
                req.textContent = this.t('art.requires', {
                    value: requirements
                        .map(item => `${item.artRequirementSummaryName || item.artRequirementSummaryId} ${item.artRequirementSummaryLevel}`)
                        .join(', ')
                });
                card.appendChild(req);
            }

            const unlocked = [...(art.artSummaryUnlockedAttackMoves || []), ...(art.artSummaryUnlockedActiveSkills || [])];
            if (unlocked.length > 0) {
                const unlockedLine = document.createElement('div');
                unlockedLine.className = 'art-line art-unlocked';
                unlockedLine.textContent = this.t('art.unlocked', { value: unlocked.join(', ') });
                card.appendChild(unlockedLine);
            }

            const next = art.artSummaryNextUnlocks || [];
            if (next.length > 0) {
                const nextLine = document.createElement('div');
                nextLine.className = 'art-line art-next';
                nextLine.textContent = this.t('art.next', { value: next.join(', ') });
                card.appendChild(nextLine);
            }

            if (!art.artSummaryIsFoundation && art.artSummaryLevel < art.artSummaryMaxLevel) {
                const train = document.createElement('button');
                train.type = 'button';
                train.className = 'art-train-btn';
                train.textContent = this.t('action.train');
                train.disabled = !this.connected || this.inBattle;
                train.addEventListener('click', () => this.sendAction({ train: art.artSummaryId }));
                card.appendChild(train);
            }

            panel.appendChild(card);
        });
    }

    percent(value, max) {
        if (!max || max <= 0) return 0;
        return Math.max(0, Math.min(100, (value / max) * 100));
    }

    setBattleMode(active, enemyName = null, enemyId = null) {
        const wasInBattle = this.inBattle;
        this.inBattle = active;
        document.body.classList.toggle('in-combat-view', active);
        this.mainContent?.classList.toggle('battle-active', active);
        this.gamePanel?.classList.toggle('battle-active', active);

        if (active) {
            if (!wasInBattle) {
                this.preBattleFilter = this.currentFilter || 'all';
                this.battleExitPending = false;
            }
            this.battlePanel.classList.remove('hidden');
            this.showCombatMessages();
            if (enemyName) {
                const enemyChanged = !wasInBattle || enemyName !== this.currentEnemy || (enemyId && enemyId !== this.currentEnemyId);
                this.currentEnemy = enemyName;
                this.currentEnemyId = enemyId || this.resolveCombatantIdByName(enemyName) || this.currentEnemyId;
                this.battleEnemyIdentity = {
                    id: this.currentEnemyId,
                    name: enemyName
                };
                this.battleEnemyName.textContent = enemyName;
                if (enemyChanged) {
                    this.resetEnemyApHud();
                    this.battleEnemyHp.style.width = '100%';
                    this.battleEnemyHpText.textContent = '-/-';
                    this.battleEnemyQiText.textContent = '-/-';
                }
            }
            this.setBattleCombatantAvatars(null, this.currentEnemyId);
        } else {
            this.clearCombatEventQueue();
            if (this.battleAnimationTimer) {
                clearTimeout(this.battleAnimationTimer);
                this.battleAnimationTimer = null;
            }
            this.battlePanel.classList.add('hidden');
            this.currentEnemy = null;
            this.currentEnemyId = null;
            this.battleEnemyIdentity = { id: null, name: null };
            this.activeEffects = [];
            this.activeSkillCooldowns = {};
            this.clearBattleMotionClasses();
            this.battlePlayerFigure?.classList.remove('fighter-defeated');
            this.battleEnemyFigure?.classList.remove('fighter-defeated');
            this.setBattleCombatantAvatars(null, null);
            this.resetEnemyApHud();
            this.renderActiveEffects();
            this.updateActiveSkillAvailability();
            this.renderArtsPanel();
            if (wasInBattle) {
                this.setMessageFilter(this.preBattleFilter || 'all');
            }
        }
        this.updateStoryGuide();
    }

    resetEnemyApHud() {
        if (this.enemyApAnimationFrame !== null) {
            window.cancelAnimationFrame(this.enemyApAnimationFrame);
            this.enemyApAnimationFrame = null;
        }
        this.enemyApDisplayValue = 0;
        this.enemyApTargetValue = 0;
        this.lastEnemyApSnapshotAt = 0;
        if (this.battleEnemyAp) {
            this.battleEnemyAp.style.width = '0%';
        }
        if (this.battleEnemyApText) {
            this.battleEnemyApText.textContent = '-';
        }
    }

    setBattleCombatantAvatars(playerId, enemyId) {
        this.setFighterAvatar(this.battlePlayerFigure, this.getCombatantAvatar(playerId, true), playerId);
        this.setFighterAvatar(this.battleEnemyFigure, this.getCombatantAvatar(enemyId, false), enemyId);
    }

    setFighterAvatar(figure, avatar, combatantId) {
        if (!figure) return;
        [...figure.classList]
            .filter(className => className.startsWith('fighter-avatar-'))
            .forEach(className => figure.classList.remove(className));
        figure.classList.add(`fighter-avatar-${avatar}`);
        if (combatantId) {
            figure.dataset.combatantId = combatantId;
        } else {
            delete figure.dataset.combatantId;
        }
    }

    getCombatantAvatar(combatantId, isPlayerSlot) {
        if (isPlayerSlot) return 'player';
        switch (combatantId) {
            case 'paper_umbrella_killer':
                return 'paper-umbrella';
            default:
                return 'opponent';
        }
    }

    resolveCombatantIdByName(name) {
        const roomCharacter = this.findCharacterByName(name);
        return roomCharacter && roomCharacter.id;
    }

    showCombatMessages() {
        this.setMessageFilter('combat');
    }

    addCombatLog(text) {
        const entry = document.createElement('div');
        entry.className = 'combat-log-entry';
        entry.textContent = `[${this.getTimeString()}] ${text}`;
        this.combatLog.appendChild(entry);
        this.combatLog.scrollTop = this.combatLog.scrollHeight;
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

    formatActiveSkillName(activeSkillId, fallback) {
        return fallback;
    }

    formatEffectName(effectId) {
        return effectId;
    }

    formatArtType(type) {
        const key = `art.type.${type}`;
        return this.hasTranslation(key) ? this.t(key) : type;
    }

    getRequirementLabels(card, requires) {
        const explicit = (card.dataset.requiresLabel || '').split(',').map(label => label.trim()).filter(Boolean);
        const allRequires = (card.dataset.requires || '').split(',').filter(Boolean);
        return requires.map((req, index) => {
            const originalIndex = allRequires.indexOf(req);
            return explicit[originalIndex >= 0 ? originalIndex : index] || this.formatEffectName(req);
        });
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
            normal: 'player_status.normal',
            in_battle: 'player_status.in_battle',
            dead: 'player_status.dead',
            banned: 'player_status.banned'
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
                        mapId: null,
                        roomId: null,
                        roomName: null,
                        position: null
                    };
                }

                if (!exit || typeof exit !== 'object') return null;
                return {
                    direction: this.normalizeDirection(exit.direction || exit.roomExitSummaryDirection),
                    mapId: exit.mapId || exit.roomExitSummaryMapId || null,
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

        if (this.inBattle) {
            const readyActiveSkill = this.getReadyActiveSkill();
            this.guideObjective.textContent = this.currentEnemy
                ? this.t('guide.defeat_enemy', { enemy: this.currentEnemy })
                : this.t('guide.defeat_current_enemy');
            if (readyActiveSkill) {
                this.guideActions.appendChild(this.createGuideNote(this.t('guide.use_skill_panel')));
            } else {
                this.guideActions.appendChild(this.createGuideNote(this.t('guide.wait_ap')));
            }
            return;
        }

        if (!quest) {
            this.guideObjective.textContent = this.currentRoom.name ? '雨夜渡口，先问掌柜。' : '等待房间信息。';
            if (this.hasCharacter('cold_rain_innkeeper')) {
                this.guideActions.appendChild(this.createGuideCharacterNote('cold_rain_innkeeper'));
            }
            return;
        }

        this.guideObjective.textContent = this.formatQuestObjective(quest) || quest.questLogEntryStage;

        if (quest.questLogEntryCompleted) {
            if (this.hasCharacter('qingyi_guest')) {
                this.guideActions.appendChild(this.createGuideCharacterNote('qingyi_guest'));
            } else if (this.hasExit('South')) {
                this.guideActions.appendChild(this.createGuideExitNote('South'));
            }
            return;
        }

        switch (quest.questLogEntryStage) {
            case 'accepted':
                if (this.hasCharacter('qingyi_guest')) {
                    this.guideActions.appendChild(this.createGuideCharacterNote('qingyi_guest'));
                } else if (this.hasExit('North')) {
                    this.guideActions.appendChild(this.createGuideExitNote('North'));
                }
                break;
            case 'witness':
                if (this.hasCharacter('paper_umbrella_killer')) {
                    this.guideActions.appendChild(this.createGuideCharacterNote('paper_umbrella_killer'));
                } else if (this.hasExit('East')) {
                    this.guideActions.appendChild(this.createGuideExitNote('East'));
                }
                break;
            case 'duel':
                if (this.hasCharacter('paper_umbrella_killer')) {
                    this.guideActions.appendChild(this.createGuideNote(
                        this.t('guide.attack_in_popup', { name: this.getCharacterDisplayName('paper_umbrella_killer') })
                    ));
                }
                break;
            default:
                if (this.currentRoom.exits.length > 0) {
                    this.guideActions.appendChild(this.createGuideNote(this.t('guide.check_exits')));
                }
        }
    }

    createGuideNote(label) {
        const note = document.createElement('span');
        note.className = 'guide-note';
        note.textContent = label;
        return note;
    }

    createGuideCharacterNote(charId) {
        return this.createGuideNote(this.t('guide.click_character', { name: this.getCharacterDisplayName(charId) }));
    }

    createGuideExitNote(direction) {
        return this.createGuideNote(this.t('guide.click_map_node', { name: this.getExitDisplayName(direction) }));
    }

    hasCharacter(charId) {
        return this.currentRoom.characters.some(char => char.id === charId);
    }

    findCharacterById(charId) {
        return this.currentRoom.characters.find(char => char.id === charId) || null;
    }

    getCharacterDisplayName(charId) {
        return this.findCharacterById(charId)?.name || charId;
    }

    hasExit(direction) {
        const normalized = this.normalizeDirection(direction);
        return this.currentRoom.exits.some(exit => this.getExitDirection(exit) === normalized);
    }

    getExitDisplayName(direction) {
        const normalized = this.normalizeDirection(direction);
        const exit = this.currentRoom.exits.find(candidate => this.getExitDirection(candidate) === normalized);
        return exit ? this.getExitLabel(exit) : this.formatDirection(normalized);
    }

    getReadyActiveSkill() {
        const { qi, ap } = this.playerStats;
        return this.currentActiveSkills.find(activeSkill => {
            const cooldown = this.activeSkillCooldowns[activeSkill.activeSkillSummaryId] || 0;
            const requires = activeSkill.activeSkillSummaryReqStatus || [];
            const hasRequirements = requires.every(req => this.activeEffects.some(effect => effect.id === req));
            return hasRequirements
                && cooldown <= 0
                && qi >= activeSkill.activeSkillSummaryCost
                && ap >= activeSkill.activeSkillSummaryApReq;
        });
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

    handleActiveSkillClick(card) {
        if (!this.connected || !this.inBattle) {
            this.displayMessage(this.t('active_skill.use.combat_only'), 'error');
            return;
        }

        const activeSkillId = card.dataset.activeSkillId;

        // Check if on cooldown
        if (card.classList.contains('on-cooldown')) {
            this.displayMessage(this.t('active_skill.use.cooldown'), 'error');
            return;
        }

        // Check if locked
        if (card.classList.contains('active-skill-locked')) {
            const requires = (card.dataset.requires || '').split(',').filter(Boolean);
            const missing = requires.filter(req => !this.activeEffects.some(e => e.id === req));
            const cost = Number(card.dataset.cost || 0);
            const apReq = Number(card.dataset.apReq || 0);
            if (missing.length > 0) {
                this.displayMessage(this.t('active_skill.use.requires', {
                    value: this.getRequirementLabels(card, missing).join(', ')
                }), 'error');
            } else if (this.playerStats.qi < cost) {
                this.displayMessage(this.t('active_skill.use.need_qi', { required: cost }), 'error');
            } else if (this.playerStats.ap < apReq) {
                this.displayMessage(this.t('active_skill.use.need_ap', { required: apReq }), 'error');
            }
            return;
        }

        this.sendAction({ perform: activeSkillId });
    }

    // Quick direction movement
    goDirection(direction) {
        if (!this.connected) return;
        this.markMapMove(direction);
        this.sendAction({ go: direction });
    }

    markMapMove(direction) {
        const normalized = this.normalizeDirection(direction);
        const targetExit = this.currentRoom.exits.find(exit => this.getExitDirection(exit) === normalized);
        const targetLabel = targetExit ? this.getExitLabel(targetExit) : this.formatDirection(normalized);
        this.pendingMapMove = {
            direction: normalized,
            roomName: targetExit && typeof targetExit === 'object' ? targetExit.roomName : targetLabel,
            roomKey: this.getExitRoomKey(targetExit, targetLabel)
        };

        if (!this.roomMap) return;
        this.roomMap.classList.add('map-moving');
        const selected = this.roomMap.querySelector(`.map-node-exit[data-direction="${normalized}"]`);
        if (selected) {
            selected.classList.add('map-node-selected');
        }

        if (this.mapMoveTimeout) {
            clearTimeout(this.mapMoveTimeout);
        }
        this.mapMoveTimeout = setTimeout(() => {
            this.roomMap?.classList.remove('map-moving');
            this.roomMap?.querySelectorAll('.map-node-selected').forEach(node => {
                node.classList.remove('map-node-selected');
            });
            this.pendingMapMove = null;
            this.mapMoveTimeout = null;
        }, 900);
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
