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
    _martialArts :: M.Map ArtId MartialArt
  }
  deriving (Eq, Show, Generic)

makeLenses ''World

data WorldException
  = MapNotFound MapId
  | RoomNotFound MapId (Int, Int)
  | ItemNotFound ItemId
  | MartialArtNotFound ArtId
  | QuestNotFound T.Text
  | CharNotFound CharId
  deriving (Eq, Show, Generic)

instance Exception WorldException
instance ToText WorldException where
  toText = \case
    MapNotFound mid -> "Map not found: " <> toText mid
    RoomNotFound mid (x, y) -> "Room not found: " <> toText mid <> " " <> toText (show (x, y))
    ItemNotFound iid -> "Item not found: " <> toText iid
    MartialArtNotFound aid -> "Martial art not found: " <> toText aid
    QuestNotFound qid -> "Quest not found: " <> toText qid
    CharNotFound cid -> "Character not found: " <> toText cid

type WorldStateT = StateT World (ExceptT WorldException IO)

loadAllAssets :: FilePath -> IO (Either T.Text World)
loadAllAssets basePath = do
  itemResult <- loadConfigFromDir _itemId $ basePath </> "items"
  martialArtResult <- loadConfigFromDir _artId $ basePath </> "martial_arts"
  effectResult <- loadConfigFromDir _effectId $ basePath </> "effects"
  charResult <- loadConfigFromDir _charId $ basePath </> "characters"
  questResult <- loadConfigFromDir _questId $ basePath </> "quests"
  mapResult <- loadConfigFromDir _mapId $ basePath </> "maps"

  return $ case (itemResult, charResult, mapResult, martialArtResult, effectResult, questResult) of
    (Right items, Right chars, Right maps, Right martialArts', Right effects, Right quests) ->
      validateWorld
        World
          { _items = items,
            _maps = maps,
            _chars = chars,
            _effects = effects,
            _quests = quests,
            _martialArts = martialArts'
          }
    _ ->
      Left $
        T.unlines
          [ fromMaybe "" $ leftToMaybe itemResult,
            fromMaybe "" $ leftToMaybe mapResult,
            fromMaybe "" $ leftToMaybe martialArtResult,
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
        <> concatMap validateCharacter (M.elems $ wrld ^. chars)
        <> concatMap validateMartialArt (M.elems $ wrld ^. martialArts)
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
            | M.notMember artId (wrld ^. martialArts)
          ]
            <> [ "item " <> item ^. itemId <> " learns non-positive art level for " <> artId
                 | level <= 0
               ]
            <> validateLearnArtLevel ("item " <> item ^. itemId <> " use learn_art") artId level

    validateCharacter char =
      [ "character " <> char ^. charId <> " teaches missing martial art " <> artId
        | artId <- M.keys $ char ^. charTeaches,
          M.notMember artId (wrld ^. martialArts)
      ]
        <> [ "character " <> char ^. charId <> " teaches non-positive max level for " <> artId
             | (artId, level) <- M.toList $ char ^. charTeaches,
               level <= 0
           ]
        <> concat
          [ validateLearnArtLevel ("character " <> char ^. charId <> " teaches") artId level
            | (artId, level) <- M.toList $ char ^. charTeaches
          ]

    validateMartialArt martialArt =
      [ "martial art " <> artId' <> " has non-positive max_level"
        | martialArt ^. artMaxLevel <= 0
      ]
        <> validateFoundationRef martialArt
        <> concatMap (validateArtRequirement artId') (martialArt ^. artRequires)
        <> concatMap (validateAttackMoveUnlock artId' $ martialArt ^. artMaxLevel) (martialArt ^. artAttackMoves)
        <> concatMap (validateActiveSkillUnlock artId' $ martialArt ^. artMaxLevel) (martialArt ^. artActiveSkills)
      where
        artId' = martialArt ^. artId

    validateFoundationRef martialArt =
      case martialArt ^. artFoundation of
        Nothing -> []
        Just foundationId ->
          case M.lookup foundationId (wrld ^. martialArts) of
            Nothing -> ["martial art " <> martialArt ^. artId <> " references missing foundation " <> foundationId]
            Just foundationArt ->
              [ "martial art " <> martialArt ^. artId <> " foundation " <> foundationId <> " is not type foundation"
                | foundationArt ^. artType /= Foundation
              ]

    validateArtRequirement ownerArtId req =
      [ "martial art " <> ownerArtId <> " requires missing martial art " <> req ^. artRequirementArt
        | M.notMember (req ^. artRequirementArt) (wrld ^. martialArts)
      ]
        <> [ "martial art " <> ownerArtId <> " has non-positive requirement level for " <> req ^. artRequirementArt
             | req ^. artRequirementLevel <= 0
           ]

    validateAttackMoveUnlock ownerArtId maxLevel attackMove =
      [ "martial art " <> ownerArtId <> " attack move " <> attackMove ^. attackMoveId <> " has non-positive unlock_level"
        | attackMove ^. attackMoveUnlockLevel <= 0
      ]
        <> [ "martial art " <> ownerArtId <> " attack move " <> attackMove ^. attackMoveId <> " unlock_level exceeds max_level"
             | attackMove ^. attackMoveUnlockLevel > maxLevel
           ]

    validateActiveSkillUnlock ownerArtId maxLevel activeSkill =
      [ "martial art " <> ownerArtId <> " active skill " <> activeSkill ^. activeSkillId <> " has non-positive unlock_level"
        | activeSkill ^. activeSkillUnlockLevel <= 0
      ]
        <> [ "martial art " <> ownerArtId <> " active skill " <> activeSkill ^. activeSkillId <> " unlock_level exceeds max_level"
             | activeSkill ^. activeSkillUnlockLevel > maxLevel
           ]
        <> [ "martial art " <> ownerArtId <> " active skill " <> activeSkill ^. activeSkillId <> " requires missing martial art " <> reqArt
             | reqArt <- activeSkill ^. activeSkillReqArts,
               M.notMember reqArt (wrld ^. martialArts)
           ]

    validateLearnArtLevel label artId level =
      case M.lookup artId (wrld ^. martialArts) of
        Nothing -> []
        Just martialArt ->
          [ label <> " level exceeds max_level for " <> artId
            | level > martialArt ^. artMaxLevel
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
      StoryMessage _ _ -> []
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
          <> validateLearnArtLevel ("quest " <> qid <> " event " <> eid <> " learn_art action") artId level
      StartBattle cid -> requireChar qid eid "start_battle action" cid

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
        | M.notMember artId (wrld ^. martialArts)
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
getsMartialArt rId = getsL martialArts rId $ MartialArtNotFound rId
