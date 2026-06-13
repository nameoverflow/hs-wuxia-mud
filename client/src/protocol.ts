export type Direction =
  | "North"
  | "South"
  | "East"
  | "West"
  | "NorthEast"
  | "NorthWest"
  | "SouthEast"
  | "SouthWest";

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
  roomId: string | null;
  roomName: string | null;
  position: [number, number] | { x: number; y: number } | null;
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

export type ActiveSkillFailureReason =
  | { reason: "need_ap"; required: number; current: number }
  | { reason: "need_qi"; required: number; current: number }
  | { reason: "cooldown"; remaining: number }
  | { reason: "missing_status"; statuses: string[] }
  | { reason: "unavailable"; activeSkillId: string };

export type ServerMessage =
  | { tag: "MoveMsg"; contents: string }
  | { tag: "ViewMsg"; contents: [string, string, unknown[], unknown[]] }
  | { tag: "AttackMsg"; contents: [string, string] }
  | { tag: "CombatNormalMsg"; contents: [string, string, CombatMessage, number] }
  | { tag: "CombatSettlementMsg"; contents: [string, string, boolean] }
  | { tag: "ActiveSkillMsg"; contents: [string, string, CombatMessage] }
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
  | { tag: "PlayerStatsMsg"; contents: [number, number, number, number, number, string] }
  | { tag: "SystemMsg"; contents: { systemMessageKey: string; systemMessageParams: Record<string, string> } }
  | { tag: "ErrorMsg"; contents: { errorSummaryCode: string; errorSummaryParams: Record<string, string> } };
