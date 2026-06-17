export type Direction =
  | "North"
  | "South"
  | "East"
  | "West"
  | "NorthEast"
  | "NorthWest"
  | "SouthEast"
  | "SouthWest";

export type RoomPosition = [number, number] | { x: number; y: number };

export type PlayerAction =
  | { go: Direction }
  | { talk: string }
  | { attack: string }
  | { perform: string }
  | { train: string }
  | { use: string }
  | { say: string }
  | { other: "view" | "quests" | "inventory" | "arts" | string };

export interface NetPlayerAction {
  tag: "NetPlayerAction";
  contents: PlayerAction;
}

export interface LoginEvent {
  tag: "Login";
  username: string;
  password: string;
}

export interface RoomCharacterSummary {
  id: string | null;
  name: string;
  desc: string;
  actions: string[];
}

export interface RoomExitSummary {
  direction: Direction;
  mapId: string | null;
  roomId: string | null;
  roomName: string | null;
  position: RoomPosition | null;
}

export interface MapRoomSummary {
  roomId: string | null;
  roomName: string;
  position: RoomPosition | null;
}

export interface MapEdgeSummary {
  direction: Direction;
  from: RoomPosition | null;
  to: RoomPosition | null;
  toRoomId: string | null;
}

export interface MapOverviewSummary {
  mapId: string;
  mapName: string;
  currentPosition: RoomPosition | null;
  rooms: MapRoomSummary[];
  edges: MapEdgeSummary[];
}

export interface EffectSummary {
  effectSummaryId: string;
  effectSummaryName: string;
  effectSummaryType: string;
  effectSummaryRemaining: number;
  effectSummaryValue: number;
}

export interface ActiveSkillSummary {
  activeSkillSummaryId: string;
  activeSkillSummaryName: string;
  activeSkillSummaryDesc: string;
  activeSkillSummaryCost: number;
  activeSkillSummaryApReq: number;
  activeSkillSummaryUnlockLevel: number;
  activeSkillSummaryCooldown: number;
  activeSkillSummaryReqStatus: string[];
  activeSkillSummaryReqStatusNames: string[];
  activeSkillSummaryDamage: number | null;
  activeSkillSummaryHeal: number | null;
}

export interface ActiveSkillCooldownSummary {
  activeSkillCooldownSummaryActiveSkillId: string;
  activeSkillCooldownSummaryRemaining: number;
}

export interface CombatantSnapshot {
  combatantSnapshotId: string;
  combatantSnapshotName: string;
  combatantSnapshotGender: string;
  combatantSnapshotCombatStyle: string;
  combatantSnapshotHp: number;
  combatantSnapshotMaxHp: number;
  combatantSnapshotQi: number;
  combatantSnapshotMaxQi: number;
  combatantSnapshotAp: number;
  combatantSnapshotEffects: EffectSummary[];
}

export interface BattleSnapshot {
  battleSnapshotPlayer: CombatantSnapshot;
  battleSnapshotEnemy: CombatantSnapshot;
  battleSnapshotActiveSkillCooldowns: ActiveSkillCooldownSummary[];
  battleSnapshotActiveSkills: ActiveSkillSummary[];
}

export type PlayerStatsLegacyPayload = [number, number, number, number, number, string];

export interface PlayerStatsSummary {
  playerStatsSummaryHp: number;
  playerStatsSummaryMaxHp: number;
  playerStatsSummaryQi: number;
  playerStatsSummaryMaxQi: number;
  playerStatsSummaryJing: number;
  playerStatsSummaryMaxJing: number;
  playerStatsSummaryAp: number;
  playerStatsSummaryStatus: string;
  playerStatsSummaryGender: string;
  playerStatsSummaryAppearance: number;
  playerStatsSummaryAppearanceText: string;
  playerStatsSummaryPortraitKey: string;
  playerStatsSummaryStrength: number;
  playerStatsSummaryAgility: number;
  playerStatsSummaryVitality: number;
}

export type PlayerStatsPayload = PlayerStatsSummary | PlayerStatsLegacyPayload;

export interface RewardSummary {
  rewardSummaryKind: string;
  rewardSummaryId: string | null;
  rewardSummaryName: string;
  rewardSummaryAmount: number;
}

export interface InventoryItemSummary {
  inventoryItemSummaryId: string;
  inventoryItemSummaryName: string;
  inventoryItemSummaryAmount: number;
  inventoryItemSummaryUsable: boolean;
}

export interface QuestLogEntry {
  questLogEntryId: string;
  questLogEntryName: string;
  questLogEntryStage: string;
  questLogEntryObjective: string | null;
  questLogEntryCompleted: boolean;
  questLogEntryRewards: RewardSummary[];
}

export interface ArtRequirementSummary {
  artRequirementSummaryId: string;
  artRequirementSummaryName: string;
  artRequirementSummaryLevel: number;
}

export interface ArtSummary {
  artSummaryId: string;
  artSummaryName: string;
  artSummaryType: string;
  artSummaryLevel: number;
  artSummaryMaxLevel: number;
  artSummaryIsFoundation: boolean;
  artSummaryFoundation: string | null;
  artSummaryRequirements: ArtRequirementSummary[];
  artSummaryUnlockedAttackMoves: string[];
  artSummaryUnlockedActiveSkills: string[];
  artSummaryNextUnlocks: string[];
}

export type CombatMessage =
  | { kind: "script"; text: string }
  | {
      kind: "effect_tick";
      effectId: string;
      effectName: string;
      effectKind: string;
      amount: number;
    };

export interface CombatVisualHint {
  actionId: string;
  tags: string[];
}

export type CombatEventKind = "normal" | "active_skill" | "effect_tick";

export type CombatResult = "hit" | "dodge" | "parry" | "effect";

export interface CombatEvent {
  kind: CombatEventKind;
  actorName: string;
  targetName: string;
  message: CombatMessage;
  damage: number | null;
  heal: number | null;
  result: CombatResult;
  visual: CombatVisualHint;
}

export type ActiveSkillFailureReason =
  | { reason: "need_ap"; required: number; current: number }
  | { reason: "need_qi"; required: number; current: number }
  | { reason: "cooldown"; remaining: number }
  | { reason: "missing_status"; statuses: string[] }
  | { reason: "unavailable"; activeSkillId: string };

export type ServerMessage =
  | { tag: "MoveMsg"; contents: string }
  | { tag: "ViewMsg"; contents: [string, string, unknown[], unknown[]] }
  | { tag: "MapOverviewMsg"; contents: MapOverviewSummary }
  | { tag: "AttackMsg"; contents: [string, string] }
  | { tag: "CombatEventMsg"; contents: CombatEvent }
  | { tag: "CombatSettlementMsg"; contents: [string, string, boolean] }
  | { tag: "ActiveSkillFailureMsg"; contents: ActiveSkillFailureReason }
  | { tag: "BattleStateMsg"; contents: BattleSnapshot }
  | { tag: "StoryMsg"; contents: [string, string] }
  | { tag: "QuestLogMsg"; contents: QuestLogEntry[] }
  | { tag: "InventoryMsg"; contents: [number, InventoryItemSummary[]] }
  | { tag: "ArtsMsg"; contents: ArtSummary[] }
  | { tag: "RewardMsg"; contents: RewardSummary[] }
  | { tag: "UseItemMsg"; contents: [string, string] }
  | { tag: "DialogueMsg"; contents: [string, string] }
  | { tag: "SayMsg"; contents: [string, string] }
  | { tag: "PlayerStatsMsg"; contents: PlayerStatsPayload }
  | { tag: "SystemMsg"; contents: { systemMessageKey: string; systemMessageParams: Record<string, string> } }
  | { tag: "ErrorMsg"; contents: { errorSummaryCode: string; errorSummaryParams: Record<string, string> } };
