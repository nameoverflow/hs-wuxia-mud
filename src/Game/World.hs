{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Game.World where

import Control.Lens
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Game.Entity
import Game.Quest
import Relude hiding (Map, show)
import System.FilePath ((</>))
import Utils

-- world properties and configures
data World = World
  { _items :: M.Map ItemId Item,
    _maps :: M.Map MapId Map,
    _chars :: M.Map CharId Character,
    _effects :: M.Map EffectId Effect,
    _quests :: M.Map T.Text Quest,
    _skills :: M.Map ArtId MartialArt
  }
  deriving (Eq, Show, Generic)

makeLenses ''World

data WorldException
  = MapNotFound MapId
  | RoomNotFound MapId (Int, Int)
  | ItemNotFound ItemId
  | SkillNotFound ArtId
  | QuestNotFound T.Text
  | CharNotFound CharId
  deriving (Eq, Show, Generic)

instance Exception WorldException
instance ToText WorldException where
  toText = \case
    MapNotFound mid -> "Map not found: " <> toText mid
    RoomNotFound mid (x, y) -> "Room not found: " <> toText mid <> " " <> toText (show (x, y))
    ItemNotFound iid -> "Item not found: " <> toText iid
    SkillNotFound sid -> "Skill not found: " <> toText sid
    QuestNotFound qid -> "Quest not found: " <> toText qid
    CharNotFound cid -> "Character not found: " <> toText cid

type WorldStateT = StateT World (ExceptT WorldException IO)

loadAllAssets :: FilePath -> IO (Either T.Text World)
loadAllAssets basePath = do
  itemResult <- loadConfigFromDir _itemId $ basePath </> "items"
  skillResult <- loadConfigFromDir _artId $ basePath </> "skills"
  effectResult <- loadConfigFromDir _effectId $ basePath </> "effects"
  charResult <- loadConfigFromDir _charId $ basePath </> "characters"
  questResult <- loadConfigFromDir _questId $ basePath </> "quests"
  mapResult <- loadConfigFromDir _mapId $ basePath </> "maps"

  return $ case (itemResult, charResult, mapResult, skillResult, effectResult, questResult) of
    (Right items, Right chars, Right maps, Right skills, Right effects, Right quests) ->
      validateWorld
        World
          { _items = items,
            _maps = maps,
            _chars = chars,
            _effects = effects,
            _quests = quests,
            _skills = skills
          }
    _ ->
      Left $
        T.unlines
          [ fromMaybe "" $ leftToMaybe itemResult,
            fromMaybe "" $ leftToMaybe mapResult,
            fromMaybe "" $ leftToMaybe skillResult,
            fromMaybe "" $ leftToMaybe effectResult,
            fromMaybe "" $ leftToMaybe questResult,
            fromMaybe "" $ leftToMaybe charResult
          ]

validateWorld :: World -> Either T.Text World
validateWorld wrld =
  case validationErrors of
    [] -> Right wrld
    errs -> Left $ T.unlines ("World validation failed:" : errs)
  where
    validationErrors =
      validateMapCharacters
        <> concatMap validateItemUse (M.elems $ wrld ^. items)
        <> concatMap validateQuestRefs (M.elems $ wrld ^. quests)

    validateMapCharacters =
      [ "map " <> mid <> " room " <> roomId' <> " references missing character " <> cid
        | (mid, mp) <- M.toList $ wrld ^. maps,
          room <- M.elems $ mp ^. mapRooms,
          cid <- room ^. roomChar,
          M.notMember cid (wrld ^. chars),
          let roomId' = room ^. roomId
      ]

    validateItemUse item =
      case item ^. itemUse of
        Nothing -> []
        Just (LearnArtUse artId level _ _ _) ->
          [ "item " <> item ^. itemId <> " use learn_art references missing martial art " <> artId
            | M.notMember artId (wrld ^. skills)
          ]
            <> [ "item " <> item ^. itemId <> " learns non-positive art level for " <> artId
                 | level <= 0
               ]

    validateQuestRefs quest =
      concatMap (validateQuestEvent $ quest ^. questId) (quest ^. questEvents)
        <> validateQuestReward (quest ^. questId) (quest ^. questReward)
        <> [ "quest " <> quest ^. questId <> " has an empty objective stage"
             | objective <- quest ^. questObjectives,
               T.null $ objective ^. questObjectiveStage
           ]

    validateQuestEvent qid event =
      validateTrigger qid (event ^. questEventId) (event ^. questEventTrigger)
        <> concatMap (validateCondition qid $ event ^. questEventId) (event ^. questEventConditions)
        <> concatMap (validateAction qid $ event ^. questEventId) (event ^. questEventActions)

    validateTrigger qid eid = \case
      TriggerTalk cid -> requireChar qid eid "talk trigger" cid
      TriggerKill cid -> requireChar qid eid "kill trigger" cid
      TriggerEnterRoom mid pos ->
        case M.lookup mid (wrld ^. maps) >>= M.lookup pos . view mapRooms of
          Just _ -> []
          Nothing -> ["quest " <> qid <> " event " <> eid <> " references missing room " <> mid <> " " <> toText (show pos)]

    validateCondition qid eid = \case
      QuestNotStarted ref -> requireQuest qid eid "condition" ref
      QuestStageIs ref _ -> requireQuest qid eid "condition" ref
      QuestCompleted ref -> requireQuest qid eid "condition" ref
      FlagSet _ -> []
      FlagUnset _ -> []
      NpcDead cid -> requireChar qid eid "condition" cid
      NpcAlive cid -> requireChar qid eid "condition" cid

    validateAction qid eid = \case
      StoryMessage _ _ choices -> concatMap (validateChoice qid eid) choices
      SetQuestStage ref _ -> requireQuest qid eid "set_stage action" ref
      CompleteQuest ref -> requireQuest qid eid "complete_quest action" ref
      SetFlag _ -> []
      ClearFlag _ -> []
      HideNpc cid -> requireChar qid eid "hide_npc action" cid
      GiveItem itemId amount ->
        requireItem qid eid "give_item action" itemId
          <> [ "quest " <> qid <> " event " <> eid <> " gives non-positive item amount for " <> itemId
               | amount <= 0
             ]
      GiveMoney amount ->
        [ "quest " <> qid <> " event " <> eid <> " gives negative money"
          | amount < 0
        ]
      LearnArt artId level ->
        requireArt qid eid "learn_art action" artId
          <> [ "quest " <> qid <> " event " <> eid <> " learns non-positive art level for " <> artId
               | level <= 0
             ]
      StartBattle cid -> requireChar qid eid "start_battle action" cid

    validateChoice qid eid choice =
      [ "quest " <> qid <> " event " <> eid <> " has an empty choice id"
        | T.null $ choice ^. storyChoiceId
      ]
        <> concatMap (validateAction qid eid) (choice ^. storyChoiceActions)

    validateQuestReward qid reward =
      concatMap validateRewardItem (reward ^. questRewardItems)
      where
        validateRewardItem rewardItem =
          requireItem qid "reward" "quest reward" (rewardItem ^. questRewardItemId)
            <> [ "quest " <> qid <> " reward has non-positive item amount for " <> rewardItem ^. questRewardItemId
                 | rewardItem ^. questRewardItemAmount <= 0
               ]

    requireQuest qid eid label ref =
      [ "quest " <> qid <> " event " <> eid <> " " <> label <> " references missing quest " <> ref
        | M.notMember ref (wrld ^. quests)
      ]

    requireChar qid eid label cid =
      [ "quest " <> qid <> " event " <> eid <> " " <> label <> " references missing character " <> cid
        | M.notMember cid (wrld ^. chars)
      ]

    requireItem qid eid label itemId =
      [ "quest " <> qid <> " event " <> eid <> " " <> label <> " references missing item " <> itemId
        | M.notMember itemId (wrld ^. items)
      ]

    requireArt qid eid label artId =
      [ "quest " <> qid <> " event " <> eid <> " " <> label <> " references missing martial art " <> artId
        | M.notMember artId (wrld ^. skills)
      ]

getsMap :: T.Text -> WorldStateT Map
getsMap mId = getsL maps mId $ MapNotFound mId

getsMapRoom :: T.Text -> (Int, Int) -> WorldStateT Room
getsMapRoom mId pos = do
  m <- getsMap mId
  case M.lookup pos $ m ^. mapRooms of
    Nothing -> throwError $ RoomNotFound mId pos
    Just room -> return room

getsCharacter :: CharId -> WorldStateT Character
getsCharacter cId = getsL chars cId $ CharNotFound cId

getsItem :: ItemId -> WorldStateT Item
getsItem iId = getsL items iId $ ItemNotFound iId

getsMartialArt :: ArtId -> WorldStateT MartialArt
getsMartialArt rId = getsL skills rId $ SkillNotFound rId
