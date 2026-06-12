{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Message where

import Game.Entity
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
  | Perform SkillId
  | Choose T.Text
  | Use T.Text
  | Say T.Text
  | Other T.Text
  deriving (Show, Eq, Generic)

instance FromJSON PlayerAction where
  parseJSON = withObject "PlayerAction" $ \o ->
    (Go <$> o .: "go")
      <|> (Attack <$> o .: "attack")
      <|> (Perform <$> o .: "perform")
      <|> (Choose <$> o .: "choose")
      <|> (Use <$> o .: "use")
      <|> (Say <$> o .: "say")
      <|> (Other <$> o .: "other")
      <|> (Talk <$> o .: "talk")


instance ToJSON PlayerAction where
  toJSON (Go dir) = object ["go" .= dir]
  toJSON (Attack targetId) = object ["attack" .= targetId]
  toJSON (Perform targetSkillId) = object ["perform" .= targetSkillId]
  toJSON (Choose choiceId) = object ["choose" .= choiceId]
  toJSON (Use usedItemId) = object ["use" .= usedItemId]
  toJSON (Say msg) = object ["say" .= msg]
  toJSON (Other msg) = object ["other" .= msg]
  toJSON (Talk targetId) = object ["talk" .= targetId]

data SkillSummary = SkillSummary
  { skillSummaryId :: SkillId,
    skillSummaryName :: T.Text,
    skillSummaryDesc :: T.Text,
    skillSummaryCost :: Int,
    skillSummaryApReq :: Int,
    skillSummaryCooldown :: Double,
    skillSummaryReqStatus :: [EffectId],
    skillSummaryReqStatusNames :: [T.Text],
    skillSummaryDamage :: Maybe Int,
    skillSummaryHeal :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON SkillSummary

data EffectSummary = EffectSummary
  { effectSummaryId :: EffectId,
    effectSummaryName :: T.Text,
    effectSummaryType :: T.Text,
    effectSummaryRemaining :: Double,
    effectSummaryValue :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON EffectSummary

data CooldownSummary = CooldownSummary
  { cooldownSummarySkillId :: SkillId,
    cooldownSummaryRemaining :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON CooldownSummary

data CombatantSnapshot = CombatantSnapshot
  { combatantSnapshotName :: T.Text,
    combatantSnapshotHp :: Int,
    combatantSnapshotMaxHp :: Int,
    combatantSnapshotQi :: Int,
    combatantSnapshotMaxQi :: Int,
    combatantSnapshotAp :: Int,
    combatantSnapshotEffects :: [EffectSummary]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CombatantSnapshot

data BattleSnapshot = BattleSnapshot
  { battleSnapshotPlayer :: CombatantSnapshot,
    battleSnapshotEnemy :: CombatantSnapshot,
    battleSnapshotCooldowns :: [CooldownSummary],
    battleSnapshotSkills :: [SkillSummary]
  }
  deriving (Show, Eq, Generic)

instance ToJSON BattleSnapshot

data StoryChoiceResp = StoryChoiceResp
  { storyChoiceRespId :: T.Text,
    storyChoiceRespText :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON StoryChoiceResp

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
    roomExitSummaryRoomId :: RoomId,
    roomExitSummaryRoomName :: T.Text,
    roomExitSummaryPosition :: (Int, Int)
  }
  deriving (Show, Eq, Generic)

instance ToJSON RoomExitSummary where
  toJSON RoomExitSummary {..} =
    object
      [ "direction" .= roomExitSummaryDirection,
        "roomId" .= roomExitSummaryRoomId,
        "roomName" .= roomExitSummaryRoomName,
        "position" .= roomExitSummaryPosition
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

data ActionResp
  -- | dst room name
  = MoveMsg T.Text
  -- | room name, room desc, visible characters, exits
  | ViewMsg T.Text T.Text [RoomCharacterSummary] [RoomExitSummary]
  -- | attacker, defender
  | AttackMsg T.Text T.Text
  -- | attacker, defender, skill desc
  | SkillMsg T.Text T.Text T.Text
  -- | attacker, defender, skill desc, damage
  | CombatNormalMsg T.Text T.Text T.Text Int
  -- | attacker, defender, win
  | CombatSettlementMsg T.Text T.Text Bool
  -- | applier, item desc
  | UseItemMsg T.Text T.Text
  -- | speaker, message
  | SayMsg T.Text T.Text
  -- | char name, message
  | DialogueMsg T.Text T.Text
  -- | hp, maxHp, qi, maxQi, ap, status
  | PlayerStatsMsg Int Int Int Int Int T.Text
  | SkillFailureMsg T.Text
  | BattleStateMsg BattleSnapshot
  | StoryMsg T.Text T.Text [StoryChoiceResp]
  | QuestLogMsg [QuestLogEntry]
  | InventoryMsg Int [InventoryItemSummary]
  | RewardMsg [RewardSummary]
  deriving (Show, Eq, Generic)

instance ToJSON ActionResp

type PlayerResp = (PlayerId, ActionResp)

formatResp :: ActionResp -> IO Text
formatResp r = return . TL.toStrict . TLE.decodeUtf8 . encode $ r
