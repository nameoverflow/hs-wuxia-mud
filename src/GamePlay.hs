{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GamePlay where

import Control.Lens
import Control.Monad (filterM, forM_, unless, when)
import qualified Control.Monad
import Control.Monad.Except
  ( MonadError (throwError)
  )
import Control.Monad.RWS (tell)
import Control.Monad.Random (getStdGen, MonadIO (liftIO), newStdGen, runRand)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Game.Combat
import Game.Entity
import Game.Message
import Game.Quest
import Game.World
import GameState
import Utils
import qualified Data.Text.IO as TIO

processPlayerAction :: PlayerId -> PlayerAction -> GameStateT ()
processPlayerAction pid action = do
  player <- getsPlayer pid
  case player ^. playerStatus of
    PlayerNormal -> processNormalAction pid action
    PlayerInBattle -> processBattleAction pid action
    _ -> return ()

processNormalAction :: PlayerId -> PlayerAction -> GameStateT ()
processNormalAction pid action = case action of
  Go direction -> playerMove pid direction
  Attack (target :: CharId) -> playerAttack pid target
  Talk (target :: CharId) -> playerTalk pid target
  Choose choiceId -> playerChooseStory pid choiceId
  Use itemId -> playerUseItem pid itemId
  Other "quests" -> playerQuestLog pid
  Other "inventory" -> sendPlayerInventory pid
  Other "view" -> playerView pid
  _ -> return ()

processBattleAction :: PlayerId -> PlayerAction -> GameStateT ()
processBattleAction pid action = case action of
  Perform skillId -> playerPerformSkill pid skillId
  Other "quests" -> playerQuestLog pid
  Other "inventory" -> sendPlayerInventory pid
  -- Use item -> do
  --   -- Update player's inventory and apply item effects
  --   players . ix playerId %= updatePlayerAfterUsingItem item
  -- Say message -> do
  --   -- Broadcast the message to other players in the same location
  --   broadcastMessage message
  -- Other command -> do
  --   -- Handle custom commands, e.g., emotes or special actions
  --   handleCustomCommand command
  _ -> return ()

playerMove :: PlayerId -> Direction -> GameStateT ()
playerMove pid direction = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer pid
  currentRoom <- liftWorld $ getsMapRoom curMapId curPos
  case currentRoom ^. roomExits . at direction of
    Nothing -> throwError $ UnableToMove direction currentRoom
    Just dst -> do
      newRoom <- liftWorld $ getsMapRoom curMapId dst
      players . at pid . _Just . playerPosition .= (curMapId, dst)
      -- broadcastMessage $ T.concat [playerId, " has entered ", newRoom ^. roomName]
      world . maps . ix curMapId . mapRooms . ix curPos . roomPlayer %= S.delete pid
      world . maps . ix curMapId . mapRooms . ix dst . roomPlayer %= S.insert pid
      tell [(pid, MoveMsg $ newRoom ^. roomName)]
      playerView pid
      _ <- runStoryTrigger pid (TriggerEnterRoom curMapId dst)
      return ()


sendPlayerStats :: PlayerId -> GameStateT ()
sendPlayerStats pid = do
  player <- getsPlayer pid
  let char = player ^. playerCharacter
  let hp = char ^. charHP
  let maxHp = char ^. charMaxHP
  let qi = char ^. charQi
  let maxQi = char ^. charMaxQi
  -- AP is only available during battle
  let ap = 0  -- Default, will be updated in battle
  let status = case player ^. playerStatus of
        PlayerNormal -> "Normal"
        PlayerInBattle -> "In Battle"
        _ -> "Unknown"
  tell [(pid, PlayerStatsMsg hp maxHp qi maxQi ap status)]

playerView :: PlayerId -> GameStateT ()
playerView pid = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer pid
  curRoom <- liftWorld $ getsMapRoom curMapId curPos
  allChars <- use $ world . chars
  curMapRooms <- use $ world . maps . ix curMapId . mapRooms
  let roomChar' = curRoom ^. roomChar
  visibleRoomCharIds <- filterM (npcVisibleToPlayer pid) roomChar'
  let curRoomChars = catMaybes $ flip map visibleRoomCharIds $ \cid ->
        case M.lookup cid allChars of
          Nothing -> Nothing
          Just char ->
            if char ^. charStatus == CharAlive
              then
                Just $
                  RoomCharacterSummary
                    { roomCharacterSummaryId = cid,
                      roomCharacterSummaryName = char ^. charName,
                      roomCharacterSummaryDesc = char ^. charDesc,
                      roomCharacterSummaryActions = map charActionText . S.toList $ char ^. charActions
                    }
              else Nothing
  let curRoomExits = catMaybes $
        flip map (M.toList $ curRoom ^. roomExits) $ \(direction, dst) ->
          case M.lookup dst curMapRooms of
            Nothing -> Nothing
            Just dstRoom ->
              Just $
                RoomExitSummary
                  { roomExitSummaryDirection = direction,
                    roomExitSummaryRoomId = dstRoom ^. roomId,
                    roomExitSummaryRoomName = dstRoom ^. roomName,
                    roomExitSummaryPosition = dst
                  }
  let curRoomName = curRoom ^. roomName
  let curRoomDesc = curRoom ^. roomDesc
  tell [(pid, ViewMsg curRoomName curRoomDesc curRoomChars curRoomExits)]
  sendPlayerStats pid
  sendPlayerInventory pid
  playerQuestLog pid
  where
    charActionText Attacking = "attack"
    charActionText Dialogue = "talk"
    charActionText Sparring = "sparring"

playerTalk :: PlayerId -> CharId -> GameStateT ()
playerTalk pid target = do
  npc <- getsRoomCharacter pid target Dialogue
  unless (charTalkable npc) $ throwError $ UnableToInteract npc Dialogue
  storyHandled <- runStoryTrigger pid (TriggerTalk target)
  unless storyHandled $ do
    -- random select a dialogue sentence
    let dialogues = npc ^. charDialogue
    randG <- newStdGen
    let (dialogue, _) = runRand (randomSelect dialogues) randG
    case dialogue of
      Nothing -> throwError $ UnableToInteract npc Dialogue
      Just dialogue' ->
        tell [(pid, DialogueMsg (npc ^. charName) dialogue')]
  where
    charTalkable :: Character -> Bool
    charTalkable char = isJust (char ^. charActions . at Dialogue) && char ^. charStatus == CharAlive

-- Create a new battle between the player and the target NPC
playerAttack :: PlayerId -> CharId -> GameStateT ()
playerAttack pid target = do
  player <- getsPlayer pid
  npc <- getsRoomCharacter pid target Attacking
  unless (charAttackable npc) $ throwError $ UnableToInteract npc Attacking
  let battle = newBattle player npc
  battles . at pid .= Just battle
  players . ix pid . playerStatus .= PlayerInBattle
  tell [(pid, AttackMsg (player ^. playerCharacter . charName) (npc ^. charName))]
  sendBattleSnapshot pid battle
  where
    charAttackable :: Character -> Bool
    charAttackable char = isJust (char ^. charActions . at Attacking) && char ^. charStatus == CharAlive

playerChooseStory :: PlayerId -> ChoiceId -> GameStateT ()
playerChooseStory pid choiceId = do
  storyState <- ensureStoryState pid
  case M.lookup choiceId (storyState ^. storyPendingChoices) of
    Nothing ->
      tell [(pid, StoryMsg "旁白" "这个选择已经随雨声散了。" [])]
    Just actions -> do
      stories . ix pid . storyPendingChoices .= M.empty
      executeStoryActions pid actions

runStoryTrigger :: PlayerId -> StoryTrigger -> GameStateT Bool
runStoryTrigger pid trigger = do
  _ <- ensureStoryState pid
  questList <- M.elems <$> use (world . quests)
  matches <- filterMStoryEvents pid trigger $ concatMap (^. questEvents) questList
  case matches of
    [] -> return False
    event : _ -> do
      executeStoryActions pid (event ^. questEventActions)
      return True

filterMStoryEvents :: PlayerId -> StoryTrigger -> [QuestEvent] -> GameStateT [QuestEvent]
filterMStoryEvents pid trigger = foldr collect (return [])
  where
    collect event acc = do
      rest <- acc
      conditionsOk <- allM (storyConditionMet pid) (event ^. questEventConditions)
      if event ^. questEventTrigger == trigger && conditionsOk
        then return (event : rest)
        else return rest

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = go
  where
    go [] = return True
    go (x : xs) = do
      ok <- f x
      if ok then go xs else return False

storyConditionMet :: PlayerId -> StoryCondition -> GameStateT Bool
storyConditionMet pid condition = do
  storyState <- ensureStoryState pid
  case condition of
    QuestNotStarted quest -> return $ M.notMember quest (storyState ^. storyQuestStages)
    QuestStageIs quest stage -> return $ storyState ^. storyQuestStages . at quest == Just stage
    QuestCompleted quest -> return $ storyState ^. storyQuestStages . at quest == Just "completed"
    FlagSet flag -> return $ S.member flag (storyState ^. storyFlags)
    FlagUnset flag -> return $ not $ S.member flag (storyState ^. storyFlags)
    NpcDead cid -> do
      npc <- liftWorld $ getsCharacter cid
      return $ npc ^. charStatus == CharDead
    NpcAlive cid -> do
      npc <- liftWorld $ getsCharacter cid
      return $ npc ^. charStatus == CharAlive

executeStoryActions :: PlayerId -> [StoryAction] -> GameStateT ()
executeStoryActions pid actions = do
  mapM_ execute actions
  if any actionChangesRoomView actions
    then playerView pid
    else do
      sendPlayerInventory pid
      playerQuestLog pid
  where
    actionChangesRoomView = \case
      HideNpc _ -> True
      _ -> False

    execute = \case
      StoryMessage speaker text choices -> do
        let choiceResponses = map (\choice -> StoryChoiceResp (choice ^. storyChoiceId) (choice ^. storyChoiceText)) choices
            pendingChoices = M.fromList $ map (\choice -> (choice ^. storyChoiceId, choice ^. storyChoiceActions)) choices
        stories . ix pid . storyPendingChoices .= pendingChoices
        tell [(pid, StoryMsg speaker text choiceResponses)]
      SetQuestStage quest stage ->
        stories . ix pid . storyQuestStages . at quest .= Just stage
      CompleteQuest quest -> do
        stories . ix pid . storyQuestStages . at quest .= Just "completed"
        grantQuestReward pid quest
      SetFlag flag ->
        stories . ix pid . storyFlags %= S.insert flag
      ClearFlag flag ->
        stories . ix pid . storyFlags %= S.delete flag
      HideNpc npc ->
        stories . ix pid . storyHiddenNpcs %= S.insert npc
      GiveItem itemId amount ->
        grantItem pid itemId amount
      GiveMoney amount ->
        grantMoney pid amount
      LearnArt artId level ->
        grantArt pid artId level
      StartBattle target ->
        playerAttack pid target

playerQuestLog :: PlayerId -> GameStateT ()
playerQuestLog pid = do
  storyState <- ensureStoryState pid
  questMap <- use $ world . quests
  itemMap <- use $ world . items
  let entries =
        [ questLogEntry itemMap quest stage
          | (qid, stage) <- M.toList $ storyState ^. storyQuestStages,
            Just quest <- [M.lookup qid questMap]
        ]
  tell [(pid, QuestLogMsg entries)]

sendPlayerInventory :: PlayerId -> GameStateT ()
sendPlayerInventory pid = do
  player <- getsPlayer pid
  itemMap <- use $ world . items
  let itemSummaries =
        [ InventoryItemSummary itemId displayName amount usable
          | (itemId, amount) <- M.toList $ player ^. playerInventory,
            amount > 0,
            let maybeItem = M.lookup itemId itemMap,
            let displayName = maybe itemId (^. itemName) maybeItem,
            let usable = maybe False (isJust . view itemUse) maybeItem
        ]
  tell [(pid, InventoryMsg (player ^. playerMoney) itemSummaries)]

playerUseItem :: PlayerId -> ItemId -> GameStateT ()
playerUseItem pid itemId = do
  player <- getsPlayer pid
  case player ^. playerInventory . at itemId of
    Just amount | amount > 0 -> do
      item <- liftWorld $ getsItem itemId
      case item ^. itemUse of
        Nothing -> throwError $ OtherException $ "Item cannot be used: " <> item ^. itemName
        Just itemUse' -> do
          applyItemUse pid player item itemUse'
          sendPlayerInventory pid
    _ -> throwError $ OtherException $ "Item not in inventory: " <> itemId

applyItemUse :: PlayerId -> Player -> Item -> ItemUse -> GameStateT ()
applyItemUse pid player item itemUse' =
  case itemUse' of
    LearnArtUse artId level shouldConsume message repeatMessage -> do
      alreadyKnown <- playerKnowsArtAtLeast pid artId level
      if alreadyKnown
        then tell [(pid, UseItemMsg (player ^. playerCharacter . charName) (fromMaybe defaultRepeatMessage repeatMessage))]
        else do
          grantArt pid artId level
          when shouldConsume $ consumeInventoryItem pid (item ^. itemId) 1
          tell [(pid, UseItemMsg (player ^. playerCharacter . charName) (fromMaybe defaultMessage message))]
  where
    itemDisplayName = item ^. itemName
    defaultMessage = "你使用了" <> itemDisplayName <> "。"
    defaultRepeatMessage = "你又翻了一遍" <> itemDisplayName <> "，其中关窍早已记在心里。"

grantQuestReward :: PlayerId -> QuestId -> GameStateT ()
grantQuestReward pid qid = do
  questMap <- use $ world . quests
  case M.lookup qid questMap of
    Nothing -> throwError $ OtherException $ "Quest not found for reward: " <> qid
    Just quest -> do
      let reward = quest ^. questReward
      when (reward ^. questRewardMoney > 0) $
        players . ix pid . playerMoney += reward ^. questRewardMoney
      forM_ (reward ^. questRewardItems) $ \rewardItem ->
        addInventoryItem pid (rewardItem ^. questRewardItemId) (rewardItem ^. questRewardItemAmount)
      summaries <- questRewardSummaries reward
      unless (null summaries) $
        tell [(pid, RewardMsg summaries)]

grantItem :: PlayerId -> ItemId -> Int -> GameStateT ()
grantItem pid itemId amount =
  when (amount > 0) $ do
    addInventoryItem pid itemId amount
    summary <- itemRewardSummary itemId amount
    tell [(pid, RewardMsg [summary])]

grantMoney :: PlayerId -> Int -> GameStateT ()
grantMoney pid amount =
  when (amount > 0) $ do
    players . ix pid . playerMoney += amount
    tell [(pid, RewardMsg [moneyRewardSummary amount])]

grantArt :: PlayerId -> ArtId -> Int -> GameStateT ()
grantArt pid artId level =
  when (level > 0) $ do
    skillMap <- use $ world . skills
    case M.lookup artId skillMap of
      Nothing -> throwError $ OtherException $ "Martial art not found for learning: " <> artId
      Just martialArt -> do
        current <- use $ players . ix pid . playerCharacter . charArt . at (martialArt ^. artType)
        let artType' = martialArt ^. artType
            bestKnownLevel [] = level
            bestKnownLevel (known : rest)
              | known ^. artDef == artId = max level (known ^. artLevel)
              | otherwise = bestKnownLevel rest
            learnedLevel = bestKnownLevel $ fromMaybe [] current
            preparedArt = ArtEntity artId learnedLevel
            upsertArt [] = [preparedArt]
            upsertArt (known : rest)
              | known ^. artDef == artId = (known & artLevel .~ learnedLevel) : rest
              | otherwise = known : upsertArt rest
        players . ix pid . playerCharacter . charArt . at artType' .= Just (upsertArt $ fromMaybe [] current)
        players . ix pid . playerCharacter . charPrepare . at artType' .= Just preparedArt
        tell [(pid, RewardMsg [martialArtRewardSummary martialArt level])]

addInventoryItem :: PlayerId -> ItemId -> Int -> GameStateT ()
addInventoryItem pid itemId amount = do
  current <- preuse $ players . ix pid . playerInventory . ix itemId
  players . ix pid . playerInventory . at itemId .= Just (fromMaybe 0 current + amount)

consumeInventoryItem :: PlayerId -> ItemId -> Int -> GameStateT ()
consumeInventoryItem pid itemId amount = do
  current <- fromMaybe 0 <$> preuse (players . ix pid . playerInventory . ix itemId)
  let remaining = current - amount
  players . ix pid . playerInventory . at itemId .= if remaining > 0 then Just remaining else Nothing

playerKnowsArtAtLeast :: PlayerId -> ArtId -> Int -> GameStateT Bool
playerKnowsArtAtLeast pid artId level = do
  skillMap <- use $ world . skills
  case M.lookup artId skillMap of
    Nothing -> throwError $ OtherException $ "Martial art not found for learning: " <> artId
    Just martialArt -> do
      player <- getsPlayer pid
      let artType' = martialArt ^. artType
      return $
        maybe
          False
          (any (\known -> known ^. artDef == artId && known ^. artLevel >= level))
          (player ^. playerCharacter . charArt . at artType')

questLogEntry :: M.Map ItemId Item -> Quest -> QuestStage -> QuestLogEntry
questLogEntry itemMap quest stage =
  QuestLogEntry
    { questLogEntryId = quest ^. questId,
      questLogEntryName = quest ^. questName,
      questLogEntryStage = stage,
      questLogEntryObjective = objectiveText stage,
      questLogEntryCompleted = stage == "completed",
      questLogEntryRewards = questRewardSummariesPure itemMap (quest ^. questReward)
    }
  where
    objectiveText currentStage =
      (^. questObjectiveText) <$> find (\objective -> objective ^. questObjectiveStage == currentStage) (quest ^. questObjectives)

questRewardSummaries :: QuestReward -> GameStateT [RewardSummary]
questRewardSummaries reward = do
  itemMap <- use $ world . items
  return $ questRewardSummariesPure itemMap reward

questRewardSummariesPure :: M.Map ItemId Item -> QuestReward -> [RewardSummary]
questRewardSummariesPure itemMap reward =
  moneyPart <> itemPart
  where
    moneyPart =
      [ moneyRewardSummary $ reward ^. questRewardMoney
        | reward ^. questRewardMoney > 0
      ]
    itemPart =
      [ RewardSummary "item" (Just itemId) displayName amount
        | rewardItem <- reward ^. questRewardItems,
          let itemId = rewardItem ^. questRewardItemId,
          let amount = rewardItem ^. questRewardItemAmount,
          amount > 0,
          let displayName = maybe itemId (^. itemName) $ M.lookup itemId itemMap
      ]

itemRewardSummary :: ItemId -> Int -> GameStateT RewardSummary
itemRewardSummary itemId amount = do
  itemMap <- use $ world . items
  let displayName = maybe itemId (^. itemName) $ M.lookup itemId itemMap
  return $ RewardSummary "item" (Just itemId) displayName amount

moneyRewardSummary :: Int -> RewardSummary
moneyRewardSummary amount = RewardSummary "money" Nothing "铜钱" amount

martialArtRewardSummary :: MartialArt -> Int -> RewardSummary
martialArtRewardSummary martialArt level = RewardSummary "martial_art" (Just $ martialArt ^. artId) (martialArt ^. artName) level

ensureStoryState :: PlayerId -> GameStateT PlayerStoryState
ensureStoryState pid = do
  maybeState <- use $ stories . at pid
  case maybeState of
    Just storyState -> return storyState
    Nothing -> do
      stories . at pid .= Just newPlayerStoryState
      return newPlayerStoryState

npcVisibleToPlayer :: PlayerId -> CharId -> GameStateT Bool
npcVisibleToPlayer pid target = do
  storyState <- ensureStoryState pid
  return $ not $ S.member target (storyState ^. storyHiddenNpcs)

getsRoomCharacter :: PlayerId -> CharId -> CharAction -> GameStateT Character
getsRoomCharacter pid target action = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer pid
  currentRoom <- liftWorld $ getsMapRoom curMapId curPos
  npc <- liftWorld $ getsCharacter target
  unless (target `elem` (currentRoom ^. roomChar)) $
    throwError $ UnableToInteract npc action
  visible <- npcVisibleToPlayer pid target
  unless visible $
    throwError $ UnableToInteract npc action
  return npc

-- Player performs a skill during battle
playerPerformSkill :: PlayerId -> SkillId -> GameStateT ()
playerPerformSkill pid targetSkillId = do
  wrld <- use world
  battle <- getsBattle pid
  player <- getsPlayer pid

  -- Get prepared technique and find the skill
  let preparedTech = player ^. playerCharacter . charPrepare . at Technique
  case preparedTech of
    Nothing -> throwError $ OtherException "No technique prepared"
    Just artEntity -> do
      let artId = artEntity ^. artDef
      case M.lookup artId (wrld ^. skills) of
        Nothing -> throwError $ OtherException $ "Martial art not found: " <> artId
        Just martialArt -> do
          -- Find the skill in the martial art
          let skillList = martialArt ^. artSkills
              maybeSkill = find (\s -> s ^. skillId == targetSkillId) skillList
          case maybeSkill of
            Nothing -> throwError $ OtherException $ "Skill not found in prepared technique: " <> targetSkillId
            Just skill -> do
              case skillCastFailure skill (battle ^. battleState) of
                Just reason -> do
                  tell [(pid, SkillFailureMsg reason)]
                  sendBattleSnapshot pid battle
                Nothing -> do
                  randG <- newStdGen
                  (_, battle', skillMsg) <- runCombat randG ExceptionInCombat wrld battle $ do
                    castSkill skill battleState battleEnemyState

                  tell skillMsg
                  let enemyDefeated = battle' ^. battleEnemyState . battleChar . charHP <= 0
                      playerDefeated = battle' ^. battleState . battleChar . charHP <= 0
                  if enemyDefeated || playerDefeated
                    then battleSettlement enemyDefeated battle'
                    else do
                      battles . at pid .= Just battle'
                      sendBattleSnapshot pid battle'

skillCastFailure :: Skill -> BattleState -> Maybe T.Text
skillCastFailure skill state
  | state ^. battleAp < skill ^. skillApReq =
      Just $ "Need " <> showText (skill ^. skillApReq) <> " AP; current AP is " <> showText (state ^. battleAp)
  | state ^. battleQi < skill ^. skillCost =
      Just $ "Need " <> showText (skill ^. skillCost) <> " Qi; current Qi is " <> showText (state ^. battleQi)
  | Just remaining <- state ^. battleSkillCd . at (skill ^. skillId) =
      Just $ "Skill is on cooldown for " <> showText (ceiling remaining :: Int) <> "s"
  | not $ all (`M.member` (state ^. battleEffects)) (skill ^. skillReqStatus) =
      Just $ "Missing required status: " <> T.intercalate ", " missingReqs
  | otherwise = Nothing
  where
    missingReqs = filter (not . (`M.member` (state ^. battleEffects))) (skill ^. skillReqStatus)
    showText = T.pack . show

-- handlePlayerInput :: T.Text -> T.Text -> GameStateT ()
-- handlePlayerInput playerId input = do
--   let action = parsePlayerAction input
--   processPlayerAction playerId action

-- updateGameState :: T.Text -> T.Text -> GameState -> IO GameState
-- updateGameState playerId input = execStateT $ handlePlayerInput playerId input

generateId :: PlayerId -> IO T.Text
generateId playerId = do
  timestamp <- getPOSIXTime
  let timestampText = T.pack . show . floor $ (timestamp :: POSIXTime)
  return $ playerId `T.append` "-" `T.append` timestampText

onGameTick :: Double -> GameStateT ()
onGameTick dt = do

  -- Update all battles
  battles' <- use battles
  forM_ (M.keys battles') $ updateBattle dt

  -- Update character respawn
  respawn . traverse %= subtract dt
  -- remove chars with respawn time <= 0 and update status to CharAlive
  respawn' <- use respawn
  let (toRespawn, toRemove) = M.partition (> 0) respawn'
  respawn .= toRespawn
  forM_ (M.keys toRemove) $ \charId -> do
    world . chars . ix charId . charStatus .= CharAlive

-- | Update the battle state
sendBattleStats :: PlayerId -> Battle -> GameStateT ()
sendBattleStats pid battle = do
  let pState = battle ^. battleState
  let char = pState ^. battleChar
  let hp = char ^. charHP
  let maxHp = char ^. charMaxHP
  let qi = pState ^. battleQi
  let maxQi = char ^. charMaxQi
  let ap = pState ^. battleAp
  let status = "In Battle"
  tell [(pid, PlayerStatsMsg hp maxHp qi maxQi ap status)]
  sendBattleSnapshot pid battle

sendBattleSnapshot :: PlayerId -> Battle -> GameStateT ()
sendBattleSnapshot pid battle = do
  wrld <- use world
  player <- getsPlayer pid
  let effectDefs = wrld ^. effects
      preparedTech = player ^. playerCharacter . charPrepare . at Technique
      preparedSkills =
        case preparedTech >>= \artEntity -> M.lookup (artEntity ^. artDef) (wrld ^. skills) of
          Nothing -> []
          Just martialArt -> map (skillToSummary effectDefs) (martialArt ^. artSkills)
      snapshot =
        BattleSnapshot
          { battleSnapshotPlayer = battleStateToSnapshot effectDefs (battle ^. battleState),
            battleSnapshotEnemy = battleStateToSnapshot effectDefs (battle ^. battleEnemyState),
            battleSnapshotCooldowns = map cooldownToSummary . M.toList $ battle ^. battleState . battleSkillCd,
            battleSnapshotSkills = preparedSkills
          }
  tell [(pid, BattleStateMsg snapshot)]
  where
    battleStateToSnapshot effectDefs state =
      let char = state ^. battleChar
       in CombatantSnapshot
            { combatantSnapshotName = char ^. charName,
              combatantSnapshotHp = char ^. charHP,
              combatantSnapshotMaxHp = char ^. charMaxHP,
              combatantSnapshotQi = state ^. battleQi,
              combatantSnapshotMaxQi = char ^. charMaxQi,
              combatantSnapshotAp = state ^. battleAp,
              combatantSnapshotEffects = map (effectToSummary effectDefs) . M.elems $ state ^. battleEffects
            }

    effectToSummary effectDefs activeEffect =
      case M.lookup (activeEffect ^. activeEffectDef) effectDefs of
        Just effect ->
          EffectSummary
            { effectSummaryId = activeEffect ^. activeEffectDef,
              effectSummaryName = effect ^. effectName,
              effectSummaryType = effectTypeText $ effect ^. effectType,
              effectSummaryRemaining = activeEffect ^. activeEffectRemaining,
              effectSummaryValue = activeEffect ^. activeEffectValue
            }
        Nothing ->
          EffectSummary
            { effectSummaryId = activeEffect ^. activeEffectDef,
              effectSummaryName = activeEffect ^. activeEffectDef,
              effectSummaryType = "unknown",
              effectSummaryRemaining = activeEffect ^. activeEffectRemaining,
              effectSummaryValue = activeEffect ^. activeEffectValue
            }

    cooldownToSummary (sid, remaining) =
      CooldownSummary
        { cooldownSummarySkillId = sid,
          cooldownSummaryRemaining = remaining
        }

    skillToSummary effectDefs skill =
      SkillSummary
        { skillSummaryId = skill ^. skillId,
          skillSummaryName = skill ^. skillName,
          skillSummaryDesc = skill ^. skillDesc,
          skillSummaryCost = skill ^. skillCost,
          skillSummaryApReq = skill ^. skillApReq,
          skillSummaryCooldown = skill ^. skillCooldown,
          skillSummaryReqStatus = skill ^. skillReqStatus,
          skillSummaryReqStatusNames = map (effectNameFor effectDefs) (skill ^. skillReqStatus),
          skillSummaryDamage = skill ^. skillDamage,
          skillSummaryHeal = skill ^. skillHeal
        }

    effectNameFor effectDefs effectId =
      maybe effectId (^. effectName) $ M.lookup effectId effectDefs

    effectTypeText DoT = "dot"
    effectTypeText HoT = "hot"
    effectTypeText Buff = "buff"
    effectTypeText DeBuff = "debuff"

updateBattle :: Double -> BattleId -> GameStateT ()
updateBattle dt bId = do
  wrld <- use world
  battle <- getsBattle bId
  -- Update battle state
  randG <- newStdGen
  (battleOver, battle', bttlMsg) <- runCombat randG ExceptionInCombat wrld battle $ flushBattleTick dt
  tell bttlMsg
  -- Send updated player stats after combat
  let pid = battle' ^. battleOwner
  sendBattleStats pid battle'
  if battleOver
    then do
      liftIO $ TIO.putStrLn "Battle over"
      let playerDefeated = battle' ^. battleState . battleChar . charHP <= 0
      battleSettlement (not playerDefeated) battle'
    else do
      -- Update battle state
      battles . at bId .= Just battle'

battleSettlement :: Bool -> Battle -> GameStateT ()
battleSettlement won battle = do
  let pChar = battle ^. battleState . battleChar
  let eChar = battle ^. battleEnemyState . battleChar
  let player = battle ^. battleOwner

  -- update status
  battles . at player .= Nothing
  players . ix player . playerStatus .= PlayerNormal
  players . ix player . playerCharacter . charHP .= max 1 (pChar ^. charHP)
  players . ix player . playerCharacter . charQi .= battle ^. battleState . battleQi

  when won $ do
    world . chars . ix (eChar ^. charId) . charStatus .= CharDead
    -- set respawn time
    respawn . at (eChar ^. charId) .= Just (fromIntegral (eChar ^. charRespawn))

  -- TODO: battle rewards

  -- send message
  tell [(player, CombatSettlementMsg player (eChar ^. charName) won)]
  when won $ do
    _ <- runStoryTrigger player (TriggerKill (eChar ^. charId))
    return ()
  sendPlayerStats player
  return ()
