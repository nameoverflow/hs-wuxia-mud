{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Message where

import Game.CharacterCreation
import Game.Entity
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text
import Control.Applicative ((<|>))

data PlayerAction
  = Go Direction
  | Talk CharId
  | Attack CharId
  | Perform ActiveSkillId
  | Train ArtId
  | Practice ArtId
  | Learn CharId ArtId Int
  | Study ItemId
  | Research ArtId
  | Meditate Int
  | EnableArt ArtType ArtId
  | PrepareArt ArtType ArtId
  | Use T.Text
  | Say T.Text
  | Other T.Text
  deriving (Show, Eq, Generic)

instance FromJSON PlayerAction where
  parseJSON = withObject "PlayerAction" $ \o ->
    (Go <$> o .: "go")
      <|> (Attack <$> o .: "attack")
      <|> (Perform <$> o .: "perform")
      <|> (Train <$> o .: "train")
      <|> (Practice <$> o .: "practice")
      <|> parseLearn o
      <|> (Study <$> o .: "study")
      <|> (Research <$> o .: "research")
      <|> (Meditate <$> o .: "meditate")
      <|> parseEnable o
      <|> parsePrepare o
      <|> (Use <$> o .: "use")
      <|> (Say <$> o .: "say")
      <|> (Other <$> o .: "other")
      <|> (Talk <$> o .: "talk")
    where
      parseLearn o = do
        learnObj <- o .: "learn"
        Learn
          <$> learnObj .: "teacher"
          <*> learnObj .: "art"
          <*> learnObj .:? "times" .!= 1

      parseEnable o = do
        enableObj <- o .: "enable"
        EnableArt
          <$> enableObj .: "type"
          <*> enableObj .: "art"

      parsePrepare o = do
        prepareObj <- o .: "prepare"
        PrepareArt
          <$> prepareObj .: "type"
          <*> prepareObj .: "art"


instance ToJSON PlayerAction where
  toJSON (Go dir) = object ["go" .= dir]
  toJSON (Attack targetId) = object ["attack" .= targetId]
  toJSON (Perform targetActiveSkillId) = object ["perform" .= targetActiveSkillId]
  toJSON (Train targetArtId) = object ["train" .= targetArtId]
  toJSON (Practice targetArtId) = object ["practice" .= targetArtId]
  toJSON (Learn teacherId targetArtId times) =
    object ["learn" .= object ["teacher" .= teacherId, "art" .= targetArtId, "times" .= times]]
  toJSON (Study itemId) = object ["study" .= itemId]
  toJSON (Research targetArtId) = object ["research" .= targetArtId]
  toJSON (Meditate amount) = object ["meditate" .= amount]
  toJSON (EnableArt artType' targetArtId) = object ["enable" .= object ["type" .= artType', "art" .= targetArtId]]
  toJSON (PrepareArt artType' targetArtId) = object ["prepare" .= object ["type" .= artType', "art" .= targetArtId]]
  toJSON (Use usedItemId) = object ["use" .= usedItemId]
  toJSON (Say msg) = object ["say" .= msg]
  toJSON (Other msg) = object ["other" .= msg]
  toJSON (Talk targetId) = object ["talk" .= targetId]

data ActiveSkillSummary = ActiveSkillSummary
  { activeSkillSummaryId :: ActiveSkillId,
    activeSkillSummaryName :: T.Text,
    activeSkillSummaryDesc :: T.Text,
    activeSkillSummaryCost :: Int,
    activeSkillSummaryApReq :: Int,
    activeSkillSummaryUnlockLevel :: Int,
    activeSkillSummaryCooldown :: Double,
    activeSkillSummaryReqStatus :: [EffectId],
    activeSkillSummaryReqStatusNames :: [T.Text],
    activeSkillSummaryDamage :: Maybe Int,
    activeSkillSummaryHeal :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ActiveSkillSummary

data ArtRequirementSummary = ArtRequirementSummary
  { artRequirementSummaryId :: ArtId,
    artRequirementSummaryName :: T.Text,
    artRequirementSummaryLevel :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ArtRequirementSummary

data ArtSummary = ArtSummary
  { artSummaryId :: ArtId,
    artSummaryName :: T.Text,
    artSummaryType :: T.Text,
    artSummaryLevel :: Int,
    artSummaryProgress :: Int,
    artSummaryNextProgress :: Int,
    artSummaryMaxLevel :: Int,
    artSummaryIsFoundation :: Bool,
    artSummaryFoundation :: Maybe ArtId,
    artSummaryRequirements :: [ArtRequirementSummary],
    artSummaryUnlockedAttackMoves :: [T.Text],
    artSummaryUnlockedActiveSkills :: [T.Text],
    artSummaryNextUnlocks :: [T.Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ArtSummary

data EffectSummary = EffectSummary
  { effectSummaryId :: EffectId,
    effectSummaryName :: T.Text,
    effectSummaryType :: T.Text,
    effectSummaryRemaining :: Double,
    effectSummaryValue :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON EffectSummary

data ActiveSkillCooldownSummary = ActiveSkillCooldownSummary
  { activeSkillCooldownSummaryActiveSkillId :: ActiveSkillId,
    activeSkillCooldownSummaryRemaining :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON ActiveSkillCooldownSummary

data CombatantSnapshot = CombatantSnapshot
  { combatantSnapshotId :: CharId,
    combatantSnapshotName :: T.Text,
    combatantSnapshotGender :: T.Text,
    combatantSnapshotCombatStyle :: T.Text,
    combatantSnapshotHp :: Int,
    combatantSnapshotMaxHp :: Int,
    combatantSnapshotQi :: Int,
    combatantSnapshotMaxQi :: Int,
    combatantSnapshotAp :: Int,
    combatantSnapshotAgility :: Int,
    combatantSnapshotEffects :: [EffectSummary]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CombatantSnapshot

data BattleSnapshot = BattleSnapshot
  { battleSnapshotPlayer :: CombatantSnapshot,
    battleSnapshotEnemy :: CombatantSnapshot,
    battleSnapshotActiveSkillCooldowns :: [ActiveSkillCooldownSummary],
    battleSnapshotActiveSkills :: [ActiveSkillSummary],
    battleSnapshotActionLockRemaining :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON BattleSnapshot

data RoomCharacterSummary = RoomCharacterSummary
  { roomCharacterSummaryId :: CharId,
    roomCharacterSummaryName :: T.Text,
    roomCharacterSummaryDesc :: T.Text,
    roomCharacterSummaryActions :: [T.Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON RoomCharacterSummary where
  toJSON RoomCharacterSummary {..} =
    object
      [ "id" .= roomCharacterSummaryId,
        "name" .= roomCharacterSummaryName,
        "desc" .= roomCharacterSummaryDesc,
        "actions" .= roomCharacterSummaryActions
      ]

data RoomExitSummary = RoomExitSummary
  { roomExitSummaryDirection :: Direction,
    roomExitSummaryMapId :: MapId,
    roomExitSummaryRoomId :: RoomId,
    roomExitSummaryRoomName :: T.Text,
    roomExitSummaryPosition :: (Int, Int)
  }
  deriving (Show, Eq, Generic)

instance ToJSON RoomExitSummary where
  toJSON RoomExitSummary {..} =
    object
      [ "direction" .= roomExitSummaryDirection,
        "mapId" .= roomExitSummaryMapId,
        "roomId" .= roomExitSummaryRoomId,
        "roomName" .= roomExitSummaryRoomName,
        "position" .= roomExitSummaryPosition
      ]

data MapRoomSummary = MapRoomSummary
  { mapRoomSummaryRoomId :: RoomId,
    mapRoomSummaryRoomName :: T.Text,
    mapRoomSummaryPosition :: (Int, Int)
  }
  deriving (Show, Eq, Generic)

instance ToJSON MapRoomSummary where
  toJSON MapRoomSummary {..} =
    object
      [ "roomId" .= mapRoomSummaryRoomId,
        "roomName" .= mapRoomSummaryRoomName,
        "position" .= mapRoomSummaryPosition
      ]

data MapEdgeSummary = MapEdgeSummary
  { mapEdgeSummaryDirection :: Direction,
    mapEdgeSummaryFromPosition :: (Int, Int),
    mapEdgeSummaryToPosition :: (Int, Int),
    mapEdgeSummaryToRoomId :: RoomId
  }
  deriving (Show, Eq, Generic)

instance ToJSON MapEdgeSummary where
  toJSON MapEdgeSummary {..} =
    object
      [ "direction" .= mapEdgeSummaryDirection,
        "from" .= mapEdgeSummaryFromPosition,
        "to" .= mapEdgeSummaryToPosition,
        "toRoomId" .= mapEdgeSummaryToRoomId
      ]

data MapOverviewSummary = MapOverviewSummary
  { mapOverviewSummaryMapId :: MapId,
    mapOverviewSummaryMapName :: T.Text,
    mapOverviewSummaryCurrentPosition :: (Int, Int),
    mapOverviewSummaryRooms :: [MapRoomSummary],
    mapOverviewSummaryEdges :: [MapEdgeSummary]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MapOverviewSummary where
  toJSON MapOverviewSummary {..} =
    object
      [ "mapId" .= mapOverviewSummaryMapId,
        "mapName" .= mapOverviewSummaryMapName,
        "currentPosition" .= mapOverviewSummaryCurrentPosition,
        "rooms" .= mapOverviewSummaryRooms,
        "edges" .= mapOverviewSummaryEdges
      ]

data RewardSummary = RewardSummary
  { rewardSummaryKind :: T.Text,
    rewardSummaryId :: Maybe T.Text,
    rewardSummaryName :: T.Text,
    rewardSummaryAmount :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON RewardSummary

data InventoryItemSummary = InventoryItemSummary
  { inventoryItemSummaryId :: ItemId,
    inventoryItemSummaryName :: T.Text,
    inventoryItemSummaryAmount :: Int,
    inventoryItemSummaryUsable :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON InventoryItemSummary

data QuestLogEntry = QuestLogEntry
  { questLogEntryId :: T.Text,
    questLogEntryName :: T.Text,
    questLogEntryStage :: T.Text,
    questLogEntryObjective :: Maybe T.Text,
    questLogEntryCompleted :: Bool,
    questLogEntryRewards :: [RewardSummary]
  }
  deriving (Show, Eq, Generic)

instance ToJSON QuestLogEntry

data SystemMessage = SystemMessage
  { systemMessageKey :: T.Text,
    systemMessageParams :: M.Map T.Text T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SystemMessage

data ErrorSummary = ErrorSummary
  { errorSummaryCode :: T.Text,
    errorSummaryParams :: M.Map T.Text T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ErrorSummary

data CombatMessage
  = CombatScriptText T.Text
  | CombatEffectTick EffectId T.Text T.Text Int
  deriving (Show, Eq, Generic)

instance ToJSON CombatMessage where
  toJSON (CombatScriptText text) =
    object
      [ "kind" .= ("script" :: T.Text),
        "text" .= text
      ]
  toJSON (CombatEffectTick effId effName effKind effAmount) =
    object
      [ "kind" .= ("effect_tick" :: T.Text),
        "effectId" .= effId,
        "effectName" .= effName,
        "effectKind" .= effKind,
        "amount" .= effAmount
      ]

data CombatEventKind
  = CombatEventNormal
  | CombatEventActiveSkill
  | CombatEventEffectTick
  deriving (Show, Eq, Generic)

combatEventKindText :: CombatEventKind -> T.Text
combatEventKindText CombatEventNormal = "normal"
combatEventKindText CombatEventActiveSkill = "active_skill"
combatEventKindText CombatEventEffectTick = "effect_tick"

data CombatResult
  = CombatHit
  | CombatDodge
  | CombatParry
  | CombatEffect
  deriving (Show, Eq, Generic)

combatResultText :: CombatResult -> T.Text
combatResultText CombatHit = "hit"
combatResultText CombatDodge = "dodge"
combatResultText CombatParry = "parry"
combatResultText CombatEffect = "effect"

data CombatEvent = CombatEvent
  { combatEventKind :: CombatEventKind,
    combatEventActorName :: T.Text,
    combatEventTargetName :: T.Text,
    combatEventMessage :: CombatMessage,
    combatEventDamage :: Maybe Int,
    combatEventHeal :: Maybe Int,
    combatEventResult :: CombatResult,
    combatEventVisual :: CombatVisualHint
  }
  deriving (Show, Eq, Generic)

instance ToJSON CombatEvent where
  toJSON CombatEvent {..} =
    object
      [ "kind" .= combatEventKindText combatEventKind,
        "actorName" .= combatEventActorName,
        "targetName" .= combatEventTargetName,
        "message" .= combatEventMessage,
        "damage" .= combatEventDamage,
        "heal" .= combatEventHeal,
        "result" .= combatResultText combatEventResult,
        "visual" .= combatEventVisual
      ]

data ActiveSkillFailureReason
  = ActiveSkillNeedAp Int Int
  | ActiveSkillNeedQi Int Int
  | ActiveSkillOnCooldown Int
  | ActiveSkillMissingStatus [EffectId]
  | ActiveSkillUnavailable ActiveSkillId
  deriving (Show, Eq, Generic)

instance ToJSON ActiveSkillFailureReason where
  toJSON (ActiveSkillNeedAp required current) =
    object
      [ "reason" .= ("need_ap" :: T.Text),
        "required" .= required,
        "current" .= current
      ]
  toJSON (ActiveSkillNeedQi required current) =
    object
      [ "reason" .= ("need_qi" :: T.Text),
        "required" .= required,
        "current" .= current
      ]
  toJSON (ActiveSkillOnCooldown remaining) =
    object
      [ "reason" .= ("cooldown" :: T.Text),
        "remaining" .= remaining
      ]
  toJSON (ActiveSkillMissingStatus missing) =
    object
      [ "reason" .= ("missing_status" :: T.Text),
        "statuses" .= missing
      ]
  toJSON (ActiveSkillUnavailable sid) =
    object
      [ "reason" .= ("unavailable" :: T.Text),
        "activeSkillId" .= sid
      ]

data PlayerStatsSummary = PlayerStatsSummary
  { playerStatsSummaryHp :: Int,
    playerStatsSummaryMaxHp :: Int,
    playerStatsSummaryQi :: Int,
    playerStatsSummaryMaxQi :: Int,
    playerStatsSummaryJing :: Int,
    playerStatsSummaryMaxJing :: Int,
    playerStatsSummaryAp :: Int,
    playerStatsSummaryStatus :: T.Text,
    playerStatsSummaryGender :: T.Text,
    playerStatsSummaryAppearance :: Int,
    playerStatsSummaryAppearanceText :: T.Text,
    playerStatsSummaryPortraitKey :: T.Text,
    playerStatsSummaryStrength :: Int,
    playerStatsSummaryAgility :: Int,
    playerStatsSummaryVitality :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PlayerStatsSummary

data ActionResp
  -- | dst room name
  = MoveMsg T.Text
  -- | room name, room desc, visible characters, exits
  | ViewMsg T.Text T.Text [RoomCharacterSummary] [RoomExitSummary]
  | MapOverviewMsg MapOverviewSummary
  -- | attacker, defender
  | AttackMsg T.Text T.Text
  | CombatEventMsg CombatEvent
  -- | attacker, defender, win
  | CombatSettlementMsg T.Text T.Text Bool
  -- | applier, item desc
  | UseItemMsg T.Text T.Text
  -- | speaker, message
  | SayMsg T.Text T.Text
  -- | char name, message
  | DialogueMsg T.Text T.Text
  | PlayerStatsMsg PlayerStatsSummary
  | ActiveSkillFailureMsg ActiveSkillFailureReason
  | BattleStateMsg BattleSnapshot
  | StoryMsg T.Text T.Text
  | StoryDelayMsg Int
  | StoryTransitionMsg T.Text Int
  | QuestLogMsg [QuestLogEntry]
  | InventoryMsg Int [InventoryItemSummary]
  | ArtsMsg [ArtSummary]
  | RewardMsg [RewardSummary]
  | CharacterCreationConfigMsg CharacterCreationConfig
  | SystemMsg SystemMessage
  | ErrorMsg ErrorSummary
  deriving (Show, Eq, Generic)

instance ToJSON ActionResp

type PlayerResp = (PlayerId, ActionResp)

formatResp :: ActionResp -> IO Text
formatResp r = return . TL.toStrict . TLE.decodeUtf8 . encode $ r
