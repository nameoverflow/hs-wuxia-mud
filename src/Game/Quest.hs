{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Quest where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.Entity
import Utils

type QuestId = Text

type QuestStage = Text

type FlagId = Text

type ChoiceId = Text

data Quest = Quest
  { _questId :: QuestId,
    _questName :: Text,
    _questObjectives :: [QuestObjective],
    _questReward :: QuestReward,
    _questEvents :: [QuestEvent]
  }
  deriving (Show, Eq, Generic)

data QuestObjective = QuestObjective
  { _questObjectiveStage :: QuestStage,
    _questObjectiveText :: Text
  }
  deriving (Show, Eq, Generic)

data QuestReward = QuestReward
  { _questRewardMoney :: Int,
    _questRewardItems :: [QuestRewardItem]
  }
  deriving (Show, Eq, Generic)

data QuestRewardItem = QuestRewardItem
  { _questRewardItemId :: ItemId,
    _questRewardItemAmount :: Int
  }
  deriving (Show, Eq, Generic)

data QuestEvent = QuestEvent
  { _questEventId :: Text,
    _questEventTrigger :: StoryTrigger,
    _questEventConditions :: [StoryCondition],
    _questEventActions :: [StoryAction]
  }
  deriving (Show, Eq, Generic)

data StoryTrigger
  = TriggerTalk CharId
  | TriggerEnterRoom MapId (Int, Int)
  | TriggerKill CharId
  deriving (Show, Eq, Generic)

data StoryCondition
  = QuestNotStarted QuestId
  | QuestStageIs QuestId QuestStage
  | QuestCompleted QuestId
  | FlagSet FlagId
  | FlagUnset FlagId
  | NpcDead CharId
  | NpcAlive CharId
  deriving (Show, Eq, Generic)

data StoryChoice = StoryChoice
  { _storyChoiceId :: ChoiceId,
    _storyChoiceText :: Text,
    _storyChoiceActions :: [StoryAction]
  }
  deriving (Show, Eq, Generic)

data StoryAction
  = StoryMessage Text Text [StoryChoice]
  | SetQuestStage QuestId QuestStage
  | CompleteQuest QuestId
  | SetFlag FlagId
  | ClearFlag FlagId
  | HideNpc CharId
  | GiveItem ItemId Int
  | GiveMoney Int
  | LearnArt ArtId Int
  | StartBattle CharId
  deriving (Show, Eq, Generic)

data PlayerStoryState = PlayerStoryState
  { _storyQuestStages :: M.Map QuestId QuestStage,
    _storyFlags :: S.Set FlagId,
    _storyHiddenNpcs :: S.Set CharId,
    _storyPendingChoices :: M.Map ChoiceId [StoryAction]
  }
  deriving (Show, Eq, Generic)

newPlayerStoryState :: PlayerStoryState
newPlayerStoryState = PlayerStoryState M.empty S.empty S.empty M.empty

makeLenses ''Quest
makeLenses ''QuestObjective
makeLenses ''QuestReward
makeLenses ''QuestRewardItem
makeLenses ''QuestEvent
makeLenses ''StoryChoice
makeLenses ''PlayerStoryState

instance FromJSON Quest where
  parseJSON = withObject "Quest" $ \o -> do
    _questId <- o .: "id"
    _questName <- o .: "name"
    _questObjectives <- o .:? "objectives" .!= []
    _questReward <- o .:? "reward" .!= emptyQuestReward
    _questEvents <- o .: "events"
    pure Quest {..}

instance Configurable Quest

emptyQuestReward :: QuestReward
emptyQuestReward = QuestReward 0 []

instance FromJSON QuestObjective where
  parseJSON = withObject "QuestObjective" $ \o -> do
    _questObjectiveStage <- o .: "stage"
    _questObjectiveText <- o .: "text"
    pure QuestObjective {..}

instance FromJSON QuestReward where
  parseJSON = withObject "QuestReward" $ \o -> do
    _questRewardMoney <- o .:? "money" .!= 0
    _questRewardItems <- o .:? "items" .!= []
    pure QuestReward {..}

instance FromJSON QuestRewardItem where
  parseJSON = withObject "QuestRewardItem" $ \o -> do
    _questRewardItemId <- o .: "id"
    _questRewardItemAmount <- o .:? "amount" .!= 1
    pure QuestRewardItem {..}

instance FromJSON QuestEvent where
  parseJSON = withObject "QuestEvent" $ \o -> do
    _questEventId <- o .: "id"
    _questEventTrigger <- o .: "trigger"
    _questEventConditions <- o .:? "conditions" .!= []
    _questEventActions <- o .: "actions"
    pure QuestEvent {..}

instance FromJSON StoryTrigger where
  parseJSON = withTypedObject "StoryTrigger" $ \typ o ->
    case typ of
      "talk" -> TriggerTalk <$> o .: "npc"
      "enter_room" -> TriggerEnterRoom <$> o .: "map" <*> o .: "position"
      "kill" -> TriggerKill <$> o .: "npc"
      _ -> fail $ "Invalid story trigger type: " <> show typ

instance FromJSON StoryCondition where
  parseJSON = withTypedObject "StoryCondition" $ \typ o ->
    case typ of
      "quest_not_started" -> QuestNotStarted <$> o .: "quest"
      "quest_stage" -> QuestStageIs <$> o .: "quest" <*> o .: "stage"
      "quest_completed" -> QuestCompleted <$> o .: "quest"
      "flag" -> FlagSet <$> o .: "flag"
      "not_flag" -> FlagUnset <$> o .: "flag"
      "npc_dead" -> NpcDead <$> o .: "npc"
      "npc_alive" -> NpcAlive <$> o .: "npc"
      _ -> fail $ "Invalid story condition type: " <> show typ

instance FromJSON StoryChoice where
  parseJSON = withObject "StoryChoice" $ \o -> do
    _storyChoiceId <- o .: "id"
    _storyChoiceText <- o .: "text"
    _storyChoiceActions <- o .:? "actions" .!= []
    pure StoryChoice {..}

instance ToJSON StoryChoice where
  toJSON StoryChoice {..} =
    object
      [ "id" .= _storyChoiceId,
        "text" .= _storyChoiceText,
        "actions" .= _storyChoiceActions
      ]

instance FromJSON StoryAction where
  parseJSON = withTypedObject "StoryAction" $ \typ o ->
    case typ of
      "message" -> StoryMessage <$> o .: "speaker" <*> o .: "text" <*> (o .:? "choices" .!= [])
      "set_stage" -> SetQuestStage <$> o .: "quest" <*> o .: "stage"
      "complete_quest" -> CompleteQuest <$> o .: "quest"
      "set_flag" -> SetFlag <$> o .: "flag"
      "clear_flag" -> ClearFlag <$> o .: "flag"
      "hide_npc" -> HideNpc <$> o .: "npc"
      "give_item" -> GiveItem <$> o .: "item" <*> (o .:? "amount" .!= 1)
      "give_money" -> GiveMoney <$> o .: "amount"
      "learn_art" -> LearnArt <$> o .: "art" <*> (o .:? "level" .!= 1)
      "start_battle" -> StartBattle <$> o .: "target"
      _ -> fail $ "Invalid story action type: " <> show typ

instance ToJSON StoryAction where
  toJSON = \case
    StoryMessage speaker text choices ->
      object ["type" .= ("message" :: Text), "speaker" .= speaker, "text" .= text, "choices" .= choices]
    SetQuestStage quest stage ->
      object ["type" .= ("set_stage" :: Text), "quest" .= quest, "stage" .= stage]
    CompleteQuest quest ->
      object ["type" .= ("complete_quest" :: Text), "quest" .= quest]
    SetFlag flag ->
      object ["type" .= ("set_flag" :: Text), "flag" .= flag]
    ClearFlag flag ->
      object ["type" .= ("clear_flag" :: Text), "flag" .= flag]
    HideNpc npc ->
      object ["type" .= ("hide_npc" :: Text), "npc" .= npc]
    GiveItem itemId amount ->
      object ["type" .= ("give_item" :: Text), "item" .= itemId, "amount" .= amount]
    GiveMoney amount ->
      object ["type" .= ("give_money" :: Text), "amount" .= amount]
    LearnArt artId level ->
      object ["type" .= ("learn_art" :: Text), "art" .= artId, "level" .= level]
    StartBattle target ->
      object ["type" .= ("start_battle" :: Text), "target" .= target]

instance FromJSON PlayerStoryState where
  parseJSON = withObject "PlayerStoryState" $ \o -> do
    _storyQuestStages <- o .:? "quest_stages" .!= M.empty
    _storyFlags <- o .:? "flags" .!= S.empty
    _storyHiddenNpcs <- o .:? "hidden_npcs" .!= S.empty
    _storyPendingChoices <- o .:? "pending_choices" .!= M.empty
    pure PlayerStoryState {..}

instance ToJSON PlayerStoryState where
  toJSON PlayerStoryState {..} =
    object
      [ "quest_stages" .= _storyQuestStages,
        "flags" .= _storyFlags,
        "hidden_npcs" .= _storyHiddenNpcs,
        "pending_choices" .= _storyPendingChoices
      ]

withTypedObject :: String -> (Text -> Object -> Parser a) -> Value -> Parser a
withTypedObject label f = withObject label $ \o -> do
  typ <- o .: "type"
  f typ o
