{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad (replicateM_, unless)
import Control.Monad.Random (mkStdGen, runRand)
import Data.Aeson (eitherDecode)
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Database
import Game.CharacterCreation
import Game.Combat
import Game.Entity
import Game.Message
import Game.Quest
import Game.World
import GamePlay
import GameState
import Networking
import Utils

main :: IO ()
main = do
  testWorldValidationCatchesBrokenQuestRefs
  testWorldValidationCatchesBrokenRoomExit
  testRandomSelectEmpty
  testDefaultFoundationArts
  testCharacterCreationChoiceAppliesInitialAttrs
  testLoginAcceptsMissingCreation
  testLoginAcceptsCreation
  testDefaultTrialSwordAnimation
  testDerivedStats
  testMoveUpdatesRoomOccupancy
  testCrossMapExitMovesPlayerBetweenMaps
  testMapOverviewIncludesCurrentMapGraph
  testCannotAttackAcrossRooms
  testNpcBattleLockBlocksConcurrentAttackAndRespawns
  testDefeatDoesNotKillNpc
  testBattleSettlementMarksPlayerDirty
  testActiveSkillFailureIsSpecific
  testActiveSkillIgnoresApAndSendsSnapshot
  testActiveSkillQueuesDuringActionLock
  testNormalAttackUsesCombatPipeline
  testMultiHitActionSplitsDamageAndLocksApproach
  testBattleActionTickDoesNotSyncApOnly
  testBattleMaintenanceDoesNotNaturallyRecoverHpOrQi
  testBattleActionLockBlocksApGrowth
  testDotEffectTicks
  testTrainRaisesFoundationAndUnlocksActiveSkills
  testProgressionActions
  testJingRequirementFailure
  testJingTickRecovery
  testLearningRequirementFailure
  testArtsQuery
  testWeiyuanChapterFlow
  testKaifengSettlingAndTravelFlow
  testStoryBattlesArePlayerIsolated
  testPlayerSaveRoundTrip
  putStrLn "All tests passed"

assert :: Bool -> String -> IO ()
assert condition message =
  unless condition $ fail message

loadFreshState :: IO GameState
loadFreshState = do
  result <- loadGameState "resources/scripts"
  case result of
    Left err -> fail $ "Failed to load game state: " <> show err
    Right gs -> pure gs

runOk :: String -> GameState -> GameStateT a -> IO ([PlayerResp], GameState)
runOk label gs action = do
  result <- runGameState gs action
  case result of
    Left err -> fail $ label <> " failed: " <> show err
    Right ok -> pure ok

requireResponseIndex :: String -> (ActionResp -> Bool) -> [PlayerResp] -> IO Int
requireResponseIndex label predicate responses =
  case findIndex (predicate . snd) responses of
    Nothing -> fail $ "missing response: " <> label
    Just index -> pure index

newTestPlayerState :: IO GameState
newTestPlayerState = do
  gs <- loadFreshState
  snd <$> runOk "create default player" gs (createDefaultPlayer "tester" "resources/scripts/default_player.yaml")

enableStarterFistForTester :: GameState -> GameState
enableStarterFistForTester =
  (players . ix "tester" . playerCharacter . charPrepare . at Fist ?~ ArtEntity "nameless_trial_fist" 1 0)
    . (players . ix "tester" . playerCharacter . charEnabled . at Fist ?~ ArtEntity "nameless_trial_fist" 1 0)

roomPlayersAtMap :: MapId -> GameState -> (Int, Int) -> S.Set PlayerId
roomPlayersAtMap targetMapId gs pos =
  case M.lookup targetMapId (gs ^. world . maps) >>= M.lookup pos . view mapRooms of
    Nothing -> S.empty
    Just room -> room ^. roomPlayer

roomPlayersAt :: GameState -> (Int, Int) -> S.Set PlayerId
roomPlayersAt = roomPlayersAtMap "weiyuan_road"

testCombatNpcId :: CharId
testCombatNpcId = "weiyuan_training_dummy"

withCombatNpcInStarterRoom :: GameState -> GameState
withCombatNpcInStarterRoom =
  world . maps . ix "weiyuan_road" . mapRooms . ix (3, 3) . roomChar %~ addNpc
  where
    addNpc npcs =
      if testCombatNpcId `elem` npcs
        then npcs
        else testCombatNpcId : npcs

withStarterExit :: Direction -> RoomRef -> GameState -> GameState
withStarterExit direction roomRef =
  world . maps . ix "weiyuan_road" . mapRooms . ix (3, 3) . roomExits . at direction ?~ roomRef

withStarterRoadRoundTrip :: GameState -> GameState
withStarterRoadRoundTrip =
  (world . maps . ix "weiyuan_road" . mapRooms . ix (3, 3) . roomExits . at East ?~ RoomRef "bianshui_road" (0, 0))
    . (world . maps . ix "bianshui_road" . mapRooms . ix (0, 0) . roomExits . at West ?~ RoomRef "weiyuan_road" (3, 3))

getNpc :: GameState -> IO Character
getNpc gs =
  case M.lookup testCombatNpcId (gs ^. world . chars) of
    Nothing -> fail "test combat NPC missing"
    Just npc -> pure npc

getBattle :: GameState -> IO Battle
getBattle gs =
  case M.lookup "tester" (gs ^. battles) of
    Nothing -> fail "tester battle missing"
    Just battle -> pure battle

startTrainingBattle :: GameState -> IO ([PlayerResp], GameState)
startTrainingBattle gs = do
  runOk "start training battle" (withCombatNpcInStarterRoom gs) (playerAttack "tester" testCombatNpcId)

questStageOf :: QuestId -> GameState -> Maybe QuestStage
questStageOf quest gs =
  M.lookup "tester" (gs ^. stories) >>= M.lookup quest . view storyQuestStages

knownArtLevel :: ArtId -> Player -> Maybe Int
knownArtLevel targetArtId player =
  case artLevels of
    [] -> Nothing
    _ -> Just $ maximum artLevels
  where
    artLevels =
      [ known ^. artLevel
        | knownArts <- M.elems $ player ^. playerCharacter . charArt,
          known <- knownArts,
          known ^. artDef == targetArtId
      ]

knowsArtAt :: ArtId -> Int -> Player -> Bool
knowsArtAt targetArtId expectedLevel player =
  knownArtLevel targetArtId player == Just expectedLevel

preparedArtIn :: ArtType -> ArtId -> Player -> Bool
preparedArtIn artType' expectedId player =
  maybe False ((== expectedId) . view artDef) (player ^. playerCharacter . charPrepare . at artType')

enabledArtIn :: ArtType -> ArtId -> Player -> Bool
enabledArtIn artType' expectedId player =
  maybe False ((== expectedId) . view artDef) (player ^. playerCharacter . charEnabled . at artType')

testWorldValidationCatchesBrokenQuestRefs :: IO ()
testWorldValidationCatchesBrokenQuestRefs = do
  gs <- loadFreshState
  case [(qid, quest) | (qid, quest) <- M.toList (gs ^. world . quests), not (null $ quest ^. questEvents)] of
    [] -> fail "test fixture has no quest events to corrupt"
    (qid, _) : _ -> do
      let broken =
            gs
              ^. world
              & quests . ix qid . questEvents . ix 0 . questEventActions %~ (++ [StartBattle "missing_npc"])
      case validateWorld broken of
        Left err ->
          assert ("missing_npc" `T.isInfixOf` err) "world validation error did not identify the missing NPC"
        Right _ -> fail "world validation accepted a quest with a missing NPC reference"

testWorldValidationCatchesBrokenRoomExit :: IO ()
testWorldValidationCatchesBrokenRoomExit = do
  gs <- loadFreshState
  case [ (mid, pos, dir)
         | (mid, mp) <- M.toList (gs ^. world . maps),
           (pos, room) <- M.toList (mp ^. mapRooms),
           (dir, _) <- M.toList (room ^. roomExits)
       ] of
    [] -> fail "test fixture has no room exits to corrupt"
    (mid, pos, dir) : _ -> do
      let broken =
            gs
              ^. world
              & maps . ix mid . mapRooms . ix pos . roomExits . ix dir . roomRefMapId .~ "missing_map"
      case validateWorld broken of
        Left err ->
          assert ("missing_map" `T.isInfixOf` err) "world validation error did not identify the missing exit map"
        Right _ -> fail "world validation accepted an exit to a missing room"

testRandomSelectEmpty :: IO ()
testRandomSelectEmpty = do
  let (selected, _) = runRand (randomSelect ([] :: [Int])) (mkStdGen 1)
  assert (selected == Nothing) "randomSelect should return Nothing for an empty list"

testDefaultFoundationArts :: IO ()
testDefaultFoundationArts = do
  gs <- newTestPlayerState
  case M.lookup "tester" (gs ^. players) of
    Nothing -> fail "tester missing"
    Just player -> do
      assert (knowsArtAt "basic_internal" 1 player) "default player is missing basic_internal"
      assert (knowsArtAt "basic_lightness" 1 player) "default player is missing basic_lightness"
      assert (knowsArtAt "basic_sword" 1 player) "default player is missing basic_sword"
      assert (knowsArtAt "basic_fist" 1 player) "default player is missing basic_fist"
      assert (knowsArtAt "nameless_trial_fist" 1 player) "default player is missing the starter fist art"
      assert (knowsArtAt "nameless_trial_sword" 1 player) "default player is missing the starter sword art"
      assert (preparedArtIn Sword "nameless_trial_sword" player) "default player did not prepare the starter sword art"
      assert (enabledArtIn Sword "nameless_trial_sword" player) "default player did not enable the starter sword art"
      assert ((player ^. playerCharacter . charPrepare . at Foundation) == Nothing) "foundation art should not be prepared"
      assert ((player ^. playerPotential) == 20) "default player potential did not load"
      assert ((player ^. playerCombatExp) == 1000) "default player combat exp did not load"
      assert ((player ^. playerCharacter . charGender) == UnknownGender) "default player gender did not load"
      assert ((player ^. playerCharacter . charAppearance) == 5) "default player appearance did not load"
      assert ((player ^. playerCharacter . charJing) == 120) "default player jing did not load"
      assert ((player ^. playerCharacter . charInnate) == InnateAttrs 18 18 18) "default player innate attrs did not load"

testCharacterCreationChoiceAppliesInitialAttrs :: IO ()
testCharacterCreationChoiceAppliesInitialAttrs = do
  gs <- loadFreshState
  let choice = CharacterCreationChoice Male "martial_family" "river_chase" "breath_lessons"
  (_, gs') <- runOk "create player with character creation" gs (createDefaultPlayerWithCreation "tester" "resources/scripts/default_player.yaml" (Just choice))
  case M.lookup "tester" (gs' ^. players) of
    Nothing -> fail "tester missing"
    Just player -> do
      assert ((player ^. playerCharacter . charInnate) == InnateAttrs 21 22 20) "character creation did not apply innate bonuses"
      assert ((player ^. playerCharacter . charMaxQi) == 124) "character creation did not apply max qi bonus"
      assert ((player ^. playerCharacter . charAppearance) == 7) "character creation did not apply appearance bonus"
      assert ((player ^. playerCharacter . charGender) == Male) "character creation did not apply gender"
      assert ((player ^. playerCharacter . charMaxHP) == (deriveStats player ^. dsMaxHp)) "character creation did not fill max hp"
      assert ((player ^. playerCharacter . charQi) == (deriveStats player ^. dsMaxQi)) "character creation did not fill initial qi"
      assert ("武学世家" `T.isInfixOf` (player ^. playerCharacter . charDesc)) "character creation did not write origin summary"
  case M.lookup "tester" (gs' ^. stories) of
    Nothing -> fail "tester story state missing after character creation"
    Just storyState ->
      assert (S.member "creation.origin.martial_family" $ storyState ^. storyFlags) "character creation origin flag was not recorded"

testLoginAcceptsMissingCreation :: IO ()
testLoginAcceptsMissingCreation =
  case eitherDecode "{\"tag\":\"Login\",\"username\":\"tester\",\"password\":\"\"}" of
    Right Login {username = "tester", password = "", creation = Nothing} -> pure ()
    other -> fail $ "legacy Login JSON did not parse: " <> show (other :: Either String NetEvent)

testLoginAcceptsCreation :: IO ()
testLoginAcceptsCreation = do
  case eitherDecode "{\"tag\":\"Login\",\"username\":\"tester\",\"password\":\"\",\"creation\":{\"gender\":\"female\",\"origin\":\"scholar_house\",\"childhood1\":\"night_reading\",\"childhood2\":\"breath_lessons\"}}" of
    Right Login {username = "tester", password = "", creation = Just (CharacterCreationChoice Female "scholar_house" "night_reading" "breath_lessons")} -> pure ()
    other -> fail $ "Login JSON with gendered creation did not parse: " <> show (other :: Either String NetEvent)
  case eitherDecode "{\"tag\":\"Login\",\"username\":\"tester\",\"password\":\"\",\"creation\":{\"origin\":\"scholar_house\",\"childhood1\":\"night_reading\",\"childhood2\":\"breath_lessons\"}}" of
    Right Login {username = "tester", password = "", creation = Just (CharacterCreationChoice UnknownGender "scholar_house" "night_reading" "breath_lessons")} -> pure ()
    other -> fail $ "legacy Login JSON with creation did not parse: " <> show (other :: Either String NetEvent)

testDefaultTrialSwordAnimation :: IO ()
testDefaultTrialSwordAnimation = do
  gs <- newTestPlayerState
  case M.lookup "tester" (gs ^. players) of
    Nothing -> fail "tester missing"
    Just player ->
      assert (combatStyleForCharacter (player ^. playerCharacter) == "sword") "default test player did not resolve to sword combat style"
  case M.lookup "nameless_trial_sword" (gs ^. world . martialArts) of
    Nothing -> fail "nameless_trial_sword missing"
    Just martialArt -> do
      let moves = martialArt ^. artAttackMoves
          moveActions = map (view $ attackMoveAnimation . animationRefAction) moves
      assert (not $ null moves) "nameless_trial_sword has no attack moves"
      assert (moveActions == ["rig.sword.thrust_a", "rig.sword.chop_a", "rig.sword.rising_cut_a"]) "trial sword moves are not bound to their fixed rig actions"
  npc <- getNpc gs
  assert (combatStyleForCharacter npc == "sword") "training dummy did not resolve to sword combat style"

testDerivedStats :: IO ()
testDerivedStats = do
  gs <- newTestPlayerState
  case M.lookup "tester" (gs ^. players) of
    Nothing -> fail "tester missing"
    Just player -> do
      let derived = deriveStats player
      assert ((derived ^. dsMaxHp) == 170) "derived max hp changed unexpectedly"
      assert ((derived ^. dsMaxQi) == 136) "derived max qi changed unexpectedly"
      assert ((derived ^. dsMaxJing) == 152) "derived max jing changed unexpectedly"
      assert ((derived ^. dsLoadLimit) == 47100) "derived load limit changed unexpectedly"
      let ironBody =
            ActiveEffect
              { _activeEffectDef = "iron_body_buff",
                _activeEffectRemaining = 10.0,
                _activeEffectValue = 5
              }
          withStatus =
            deriveCharacterStats
              (player ^. playerCharacter)
              (player ^. playerCombatExp)
              (characterDerivedStatSources (gs ^. world . effects) (M.singleton "iron_body_buff" ironBody) (player ^. playerCharacter))
      assert ((withStatus ^. dsStrength) == 23) "status modifier did not affect derived strength"
      assert ((withStatus ^. dsDamageReduction) > (derived ^. dsDamageReduction)) "status modifier did not affect mitigation"

testMoveUpdatesRoomOccupancy :: IO ()
testMoveUpdatesRoomOccupancy = do
  gs <- withStarterExit North (RoomRef "kaifeng_city" (0, -3)) <$> newTestPlayerState
  (_, atKaifengEntry) <- runOk "move north to Kaifeng entry" gs (playerMove "tester" North)
  (_, moved) <- runOk "move north inside Kaifeng" atKaifengEntry (playerMove "tester" North)
  assert (not $ S.member "tester" (roomPlayersAtMap "kaifeng_city" moved (0, -3))) "player remained in the old room after moving"
  assert (S.member "tester" (roomPlayersAtMap "kaifeng_city" moved (0, -2))) "player was not added to the new room after moving"

testCrossMapExitMovesPlayerBetweenMaps :: IO ()
testCrossMapExitMovesPlayerBetweenMaps = do
  gs <- withStarterRoadRoundTrip <$> newTestPlayerState
  (responses, inMountainPass) <- runOk "move east to official road" gs (playerMove "tester" East)
  case M.lookup "tester" (inMountainPass ^. players) of
    Nothing -> fail "tester missing after cross-map movement"
    Just player ->
      assert ((player ^. playerPosition) == ("bianshui_road", (0, 0))) "player position did not switch to the target map"
  assert (not $ S.member "tester" (roomPlayersAt inMountainPass (3, 3))) "player remained in the source map room"
  assert (S.member "tester" (roomPlayersAtMap "bianshui_road" inMountainPass (0, 0))) "player was not added to the target map room"
  case [exits | (_, ViewMsg _ _ _ exits) <- responses] of
    [] -> fail "cross-map movement did not send a room view"
    exits : _ ->
      assert
        (any (\exit -> roomExitSummaryMapId exit == "weiyuan_road" && roomExitSummaryPosition exit == (3, 3)) exits)
        "target room view did not preserve the cross-map return exit"

  (_, returned) <- runOk "return west to test map" inMountainPass (playerMove "tester" West)
  case M.lookup "tester" (returned ^. players) of
    Nothing -> fail "tester missing after returning from cross-map movement"
    Just player ->
      assert ((player ^. playerPosition) == ("weiyuan_road", (3, 3))) "player did not return to the source map"

testMapOverviewIncludesCurrentMapGraph :: IO ()
testMapOverviewIncludesCurrentMapGraph = do
  gs <- newTestPlayerState
  (responses, _) <- runOk "map overview" gs (playerMapOverview "tester")
  case [overview | (_, MapOverviewMsg overview) <- responses] of
    [overview] -> do
      currentMap <-
        case M.lookup (mapOverviewSummaryMapId overview) (gs ^. world . maps) of
          Nothing -> fail "current map missing from world fixture"
          Just mp -> pure mp
      assert (mapOverviewSummaryMapId overview == "weiyuan_road") "map overview did not use the player's current map"
      assert (mapOverviewSummaryMapName overview == currentMap ^. mapName) "map overview did not include the map name"
      assert (mapOverviewSummaryCurrentPosition overview == (3, 3)) "map overview did not include the player's current position"
      assert
        (map mapRoomSummaryRoomId (mapOverviewSummaryRooms overview) == map (view roomId . snd) (M.toAscList $ currentMap ^. mapRooms))
        "map overview did not include every room in coordinate order"
      let expectedLocalEdges =
            [ ()
              | (fromPosition, room) <- M.toList (currentMap ^. mapRooms),
                (_direction, exitRef) <- M.toList (room ^. roomExits),
                exitRef ^. roomRefMapId == mapOverviewSummaryMapId overview,
                M.member fromPosition (currentMap ^. mapRooms)
            ]
      assert
        (length (mapOverviewSummaryEdges overview) == length expectedLocalEdges)
        "map overview did not include the expected local edges"
      assert
        (not $ any ((== (0, 0)) . mapEdgeSummaryToPosition) (mapOverviewSummaryEdges overview))
        "map overview should not include cross-map edges in the current map graph"
    [] -> fail "map overview did not send MapOverviewMsg"
    _ -> fail "map overview sent multiple MapOverviewMsg responses"

testCannotAttackAcrossRooms :: IO ()
testCannotAttackAcrossRooms = do
  gs <- withStarterExit East (RoomRef "bianshui_road" (0, 0)) . withCombatNpcInStarterRoom <$> newTestPlayerState
  (_, moved) <- runOk "move east" gs (playerMove "tester" East)
  result <- runGameState moved (playerAttack "tester" testCombatNpcId)
  case result of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected UnableToInteract Attacking, got: " <> show err
    Right _ -> fail "player attacked an NPC from a different room"

testNpcBattleLockBlocksConcurrentAttackAndRespawns :: IO ()
testNpcBattleLockBlocksConcurrentAttackAndRespawns = do
  gs <- loadFreshState
  (_, withTester) <- runOk "create tester" gs (createDefaultPlayer "tester" "resources/scripts/default_player.yaml")
  (_, withRival) <- runOk "create rival" withTester (createDefaultPlayer "rival" "resources/scripts/default_player.yaml")
  let withCombatNpc = withCombatNpcInStarterRoom withRival
  (_, locked) <- runOk "tester starts npc battle" withCombatNpc (playerAttack "tester" testCombatNpcId)
  npcLocked <- getNpc locked
  assert ((npcLocked ^. charStatus) == CharBattle) "NPC was not locked when battle started"

  concurrentAttack <- runGameState locked (playerAttack "rival" testCombatNpcId)
  case concurrentAttack of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected concurrent NPC attack to be blocked, got: " <> show err
    Right _ -> fail "second player started a battle against a locked NPC"

  let defeatedPlayer =
        locked
          & battles . ix "tester" . battleState . battleChar . charHP .~ 0
  (_, released) <- runOk "tester loses and releases npc" defeatedPlayer (updateBattle 0 "tester")
  npcReleased <- getNpc released
  assert ((npcReleased ^. charStatus) == CharAlive) "NPC lock was not released after player defeat"

  (_, rivalBattle) <- runOk "rival starts npc battle after release" released (playerAttack "rival" testCombatNpcId)
  npcLockedAgain <- getNpc rivalBattle
  assert ((npcLockedAgain ^. charStatus) == CharBattle) "NPC was not locked for the second battle"

  let defeatedEnemy =
        rivalBattle
          & battles . ix "rival" . battleEnemyState . battleChar . charHP .~ 0
  (_, dead) <- runOk "rival kills npc" defeatedEnemy (updateBattle 0 "rival")
  npcDead <- getNpc dead
  assert ((npcDead ^. charStatus) == CharDead) "NPC did not stay dead after being killed"

  attackDeadNpc <- runGameState dead (playerAttack "tester" testCombatNpcId)
  case attackDeadNpc of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected dead NPC attack to be blocked, got: " <> show err
    Right _ -> fail "player attacked a dead NPC before respawn"

  (_, respawned) <- runOk "respawn locked npc" dead (tickRespawns 5)
  npcRespawned <- getNpc respawned
  let npcStats = deriveCharacterStats npcRespawned 0 emptyDerivedStatSources
  assert ((npcRespawned ^. charStatus) == CharAlive) "NPC did not respawn as alive"
  assert ((npcRespawned ^. charHP) == npcStats ^. dsMaxHp) "NPC did not respawn at full HP"
  assert ((npcRespawned ^. charQi) == npcStats ^. dsMaxQi) "NPC did not respawn at full Qi"

testDefeatDoesNotKillNpc :: IO ()
testDefeatDoesNotKillNpc = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let defeated =
        inBattle
          & battles . ix "tester" . battleState . battleChar . charHP .~ 0
  (_, settled) <- runOk "settle defeated battle" defeated (updateBattle 0 "tester")
  assert (M.notMember "tester" (settled ^. battles)) "battle was not cleared after defeat"
  npc <- getNpc settled
  assert ((npc ^. charStatus) == CharAlive) "NPC was killed when the player lost"
  assert (M.notMember testCombatNpcId (settled ^. respawn)) "NPC respawn was scheduled when the player lost"
  case M.lookup "tester" (settled ^. players) of
    Nothing -> fail "tester missing after defeat"
    Just player -> do
      assert ((player ^. playerStatus) == PlayerNormal) "player was not returned to normal status after defeat"
      assert ((player ^. playerCharacter . charHP) == 1) "defeated player should be left at 1 HP"

testBattleSettlementMarksPlayerDirty :: IO ()
testBattleSettlementMarksPlayerDirty = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  (_, ticking) <- runOk "ordinary battle tick" inBattle (updateBattle 0 "tester")
  assert (S.notMember "tester" (ticking ^. dirtyPlayers)) "ordinary battle tick should not mark the player dirty"
  let defeatedEnemy =
        inBattle
          & battles . ix "tester" . battleEnemyState . battleChar . charHP .~ 0
  (_, settled) <- runOk "settle won battle" defeatedEnemy (updateBattle 0 "tester")
  assert (S.member "tester" (settled ^. dirtyPlayers)) "battle settlement did not mark the player dirty"

testActiveSkillFailureIsSpecific :: IO ()
testActiveSkillFailureIsSpecific = do
  gs <- enableStarterFistForTester <$> newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let drained =
        inBattle
          & battles . ix "tester" . battleState . battleQi .~ 0
  (responses, _) <- runOk "perform active skill without qi" drained (playerPerformActiveSkill "tester" "power_strike")
  assert
    (any (\(_, resp) -> resp == ActiveSkillFailureMsg (ActiveSkillNeedQi 30 0)) responses)
    "active skill failure did not report the specific Qi requirement"

testActiveSkillIgnoresApAndSendsSnapshot :: IO ()
testActiveSkillIgnoresApAndSendsSnapshot = do
  gs <- enableStarterFistForTester <$> newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let ready =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 0
  (responses, afterSkill) <- runOk "perform power strike" ready (playerPerformActiveSkill "tester" "power_strike")
  battle <- getBattle afterSkill
  assert ((battle ^. battleState . battleAp) == 0) "active skill changed AP"
  assert ((battle ^. battleState . battleQi) == 70) "active skill did not consume Qi"
  assert ((battle ^. battleEnemyState . battleChar . charHP) == 77) "active skill did not apply derived damage"
  assert (any isActiveSkillEvent responses) "active skill success did not emit a combat event"
  assert (any (isBattleStateMsg . snd) responses) "active skill success did not send a battle snapshot"
  where
    isActiveSkillEvent (_, CombatEventMsg event) =
      combatEventKind event == CombatEventActiveSkill
        && not (T.null $ combatEventActorName event)
        && not (T.null $ combatEventTargetName event)
        && combatEventDamage event == Just 37
        && not (T.null $ combatEventVisual event ^. combatVisualActionId)
    isActiveSkillEvent _ = False

    isBattleStateMsg (BattleStateMsg _) = True
    isBattleStateMsg _ = False

testActiveSkillQueuesDuringActionLock :: IO ()
testActiveSkillQueuesDuringActionLock = do
  gs <- enableStarterFistForTester <$> newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let locked =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 0
          & battles . ix "tester" . battleActionLockRemaining .~ 0.5
  (queueResponses, queued) <- runOk "queue skill while action lock is active" locked (playerPerformActiveSkill "tester" "power_strike")
  queuedBattle <- getBattle queued
  assert (not $ any (isCombatEvent . snd) queueResponses) "queued active skill executed before action lock expired"
  assert (queuedBattle ^. battlePendingActiveSkill /= Nothing) "active skill was not queued during action lock"
  assert ((queuedBattle ^. battleState . battleQi) == 100) "queued active skill consumed Qi before execution"

  (blockedResponses, stillQueued) <- runOk "tick before queued skill can fire" queued (onBattleTick 0.25)
  stillQueuedBattle <- getBattle stillQueued
  assert (null blockedResponses) "locked queued skill tick emitted responses too early"
  assert (stillQueuedBattle ^. battlePendingActiveSkill /= Nothing) "queued active skill fired before lock expired"

  (skillResponses, afterSkill) <- runOk "release queued active skill" stillQueued (onBattleTick 0.5)
  battle <- getBattle afterSkill
  assert (any isActiveSkillEvent skillResponses) "queued active skill did not emit a combat event"
  assert (battle ^. battlePendingActiveSkill == Nothing) "queued active skill was not cleared"
  assert ((battle ^. battleState . battleAp) == 0) "queued active skill changed AP"
  assert ((battle ^. battleState . battleQi) == 70) "queued active skill did not consume Qi on execution"
  assert ((battle ^. battleEnemyState . battleChar . charHP) == 77) "queued active skill did not apply damage"
  assert ((battle ^. battleActionLockRemaining) > 0) "queued active skill did not set an action lock"
  where
    isCombatEvent (CombatEventMsg _) = True
    isCombatEvent _ = False

    isActiveSkillEvent (_, CombatEventMsg event) =
      combatEventKind event == CombatEventActiveSkill
    isActiveSkillEvent _ = False

testNormalAttackUsesCombatPipeline :: IO ()
testNormalAttackUsesCombatPipeline = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let ready =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 100
          & battles . ix "tester" . battleState . battleChar . charInnate . innateStrength .~ 60
          & battles . ix "tester" . battleEnemyState . battleEffects . at "weakened" ?~ ActiveEffect "weakened" 5.0 100
          & battles . ix "tester" . battleEnemyState . battleChar . charPrepare .~ M.empty
  (responses, afterTick) <- runOk "normal attack pipeline tick" ready (updateBattle 0 "tester")
  let combatDamages =
        [ dmg
          | (_, CombatEventMsg event) <- responses,
            Just dmg <- [combatEventDamage event],
            dmg > 0
        ]
  case combatDamages of
    [] -> fail "normal attack pipeline did not emit a damaging combat message"
    damage : _ -> do
      expectedDamages <- expectedNormalAttackDamages ready
      assert (damage `elem` expectedDamages) "normal attack damage did not use derived strength and mitigation"
      battle <- getBattle afterTick
      assert ((battle ^. battleEnemyState . battleChar . charHP) == 114 - damage) "normal attack damage was not applied to the enemy"

testMultiHitActionSplitsDamageAndLocksApproach :: IO ()
testMultiHitActionSplitsDamageAndLocksApproach = do
  assert (splitByShares 18 [1, 1, 2] == [5, 4, 9]) "hit shares must split like the client"
  assert (sum (splitByShares 7 [1, 1, 1]) == 7) "hit shares must preserve the total"
  gs <- newTestPlayerState
  case M.lookup "rig.fist.combo_a" (gs ^. world . combatActionTimings) of
    Nothing -> fail "combo action timing was not loaded from the manifest"
    Just timing -> do
      assert ((timing ^. combatActionTimingApproachMs) == 280) "approach duration was not read from the manifest"
      assert ((timing ^. combatActionTimingHitShares) == [1, 1, 2]) "hit shares were not read from the manifest"
  let comboWorld = gs & world . martialArts . traverse . artAttackMoves . traverse . attackMoveAnimation . animationRefAction .~ "rig.fist.combo_a"
  (_, inBattle) <- startTrainingBattle comboWorld
  let ready =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 100
          & battles . ix "tester" . battleEnemyState . battleAp .~ 0
          & battles . ix "tester" . battleActionLockRemaining .~ 0
  enemyHpBefore <- (^. battleEnemyState . battleChar . charHP) <$> getBattle ready
  (responses, afterTick) <- runOk "multi-hit attack tick" ready (updateBattle 0 "tester")
  let comboEvents = [event | (_, CombatEventMsg event) <- responses, length (combatEventHits event) == 3]
  case comboEvents of
    [] -> fail "multi-hit attack did not report three hit outcomes"
    event : _ -> do
      let landed = sum [damage | CombatHitOutcome CombatHit (Just damage) _ <- combatEventHits event]
      assert (combatEventDamage event == Just landed) "event damage must equal the landed hits"
      battle <- getBattle afterTick
      assert ((battle ^. battleEnemyState . battleChar . charHP) == enemyHpBefore - landed) "landed hits were not applied to the enemy"
      assert ((battle ^. battleActionLockRemaining) >= 1.0) "action lock must cover the approach plus the clip"

testBattleActionTickDoesNotSyncApOnly :: IO ()
testBattleActionTickDoesNotSyncApOnly = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  (responses, afterTick) <- runOk "fast battle ap-only tick" inBattle (onBattleTick 0.05)
  battle <- getBattle afterTick
  assert (null responses) "AP-only battle tick should not send responses"
  assert ((battle ^. battleState . battleAp) > 0) "fast battle tick did not accumulate player AP"
  assert ((battle ^. battleEnemyState . battleAp) > 0) "fast battle tick did not accumulate enemy AP"

testBattleMaintenanceDoesNotNaturallyRecoverHpOrQi :: IO ()
testBattleMaintenanceDoesNotNaturallyRecoverHpOrQi = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let wounded =
        inBattle
          & battles . ix "tester" . battleState . battleChar . charHP .~ 40
          & battles . ix "tester" . battleState . battleQi .~ 10
          & battles . ix "tester" . battleEnemyState . battleChar . charHP .~ 50
          & battles . ix "tester" . battleEnemyState . battleQi .~ 12
  (_, afterTick) <- runOk "battle maintenance without natural recovery" wounded (tickBattleMaintenance 1)
  battle <- getBattle afterTick
  assert ((battle ^. battleState . battleChar . charHP) == 40) "player HP naturally recovered during battle"
  assert ((battle ^. battleState . battleQi) == 10) "player Qi naturally recovered during battle"
  assert ((battle ^. battleEnemyState . battleChar . charHP) == 50) "enemy HP naturally recovered during battle"
  assert ((battle ^. battleEnemyState . battleQi) == 12) "enemy Qi naturally recovered during battle"

testBattleActionLockBlocksApGrowth :: IO ()
testBattleActionLockBlocksApGrowth = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let ready =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 100
          & battles . ix "tester" . battleEnemyState . battleAp .~ 20
  (actionResponses, locked) <- runOk "trigger normal attack lock" ready (onBattleTick 0)
  assert (any (isCombatEvent . snd) actionResponses) "ready combatant did not emit an action"
  lockedBattle <- getBattle locked
  let playerApAfterAction = lockedBattle ^. battleState . battleAp
      enemyApAfterAction = lockedBattle ^. battleEnemyState . battleAp
      lockRemaining = lockedBattle ^. battleActionLockRemaining
  assert (lockRemaining > 0) "normal attack did not set an action lock"

  (blockedResponses, blocked) <- runOk "tick during action lock" locked (onBattleTick 0.05)
  blockedBattle <- getBattle blocked
  assert (null blockedResponses) "action-locked tick should not emit responses"
  assert ((blockedBattle ^. battleState . battleAp) == playerApAfterAction) "player AP grew during action lock"
  assert ((blockedBattle ^. battleEnemyState . battleAp) == enemyApAfterAction) "enemy AP grew during action lock"
  assert ((blockedBattle ^. battleActionLockRemaining) < lockRemaining) "action lock did not tick down"

  (_, resumed) <- runOk "tick after action lock" blocked (onBattleTick (lockRemaining + 0.2))
  resumedBattle <- getBattle resumed
  assert ((resumedBattle ^. battleActionLockRemaining) == 0) "action lock did not expire"
  assert
    ((resumedBattle ^. battleState . battleAp) > playerApAfterAction || (resumedBattle ^. battleEnemyState . battleAp) > enemyApAfterAction)
    "AP did not resume after action lock expired"
  where
    isCombatEvent (CombatEventMsg _) = True
    isCombatEvent _ = False

expectedNormalAttackDamages :: GameState -> IO [Int]
expectedNormalAttackDamages gs = do
  battle <- getBattle gs
  let effectDefs = gs ^. world . effects
      attackerState = battle ^. battleState
      defenderState = battle ^. battleEnemyState
      attackerChar = attackerState ^. battleChar
      defenderChar = defenderState ^. battleChar
      attackerStats =
        deriveCharacterStats
          attackerChar
          (attackerState ^. battleCombatExp)
          (characterDerivedStatSources effectDefs (attackerState ^. battleEffects) attackerChar)
      defenderStats =
        deriveCharacterStats
          defenderChar
          (defenderState ^. battleCombatExp)
          (characterDerivedStatSources effectDefs (defenderState ^. battleEffects) defenderChar)
  pure $
    [ computeDamage attackerStats defenderStats preparedAttack
      | preparedAttack <- unlockedPreparedAttacks (gs ^. world . martialArts) attackerChar
    ]

testDotEffectTicks :: IO ()
testDotEffectTicks = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let bleeding =
        ActiveEffect
          { _activeEffectDef = "bleeding",
            _activeEffectRemaining = 2.0,
            _activeEffectValue = 5
          }
      withBleed =
        inBattle
          & battles . ix "tester" . battleEnemyState . battleEffects . at "bleeding" ?~ bleeding
  (_, afterTick) <- runOk "tick bleeding" withBleed (updateBattle 1 "tester")
  battle <- getBattle afterTick
  assert ((battle ^. battleEnemyState . battleChar . charHP) == 109) "DoT did not damage the affected combatant"
  case M.lookup "bleeding" (battle ^. battleEnemyState . battleEffects) of
    Nothing -> fail "DoT effect disappeared too early"
    Just effect ->
      assert ((effect ^. activeEffectRemaining) == 1.0) "DoT remaining duration did not tick down"

testTrainRaisesFoundationAndUnlocksActiveSkills :: IO ()
testTrainRaisesFoundationAndUnlocksActiveSkills = do
  gs <- newTestPlayerState
  (_, learned) <- runOk "learn weiyuan sword" gs (grantArt "tester" "weiyuan_sword" 1)

  (lowBattleMsgs, _) <- startTrainingBattle learned
  let lowActiveSkillIds = battleActiveSkillIds lowBattleMsgs
  assert ("steady_cut" `elem` lowActiveSkillIds) "level 1 weiyuan_sword did not expose steady_cut"
  assert ("eight_direction_thrusts" `notElem` lowActiveSkillIds) "eight_direction_thrusts unlocked before level 5"

  (_, trained) <- runOk "train weiyuan sword to 5" learned $
    replicateM_ 4 (playerTrainArt "tester" "weiyuan_sword")
  case M.lookup "tester" (trained ^. players) of
    Nothing -> fail "tester missing after training"
    Just player -> do
      assert (knowsArtAt "weiyuan_sword" 5 player) "training did not raise weiyuan_sword to level 5"
      assert (knowsArtAt "basic_sword" 5 player) "training did not raise basic_sword to level 5"
      assert (preparedArtIn Sword "weiyuan_sword" player) "training did not keep weiyuan_sword prepared"

  (highBattleMsgs, _) <- startTrainingBattle trained
  let highActiveSkillIds = battleActiveSkillIds highBattleMsgs
  assert ("eight_direction_thrusts" `elem` highActiveSkillIds) "eight_direction_thrusts did not unlock at level 5"
  where
    battleActiveSkillIds responses =
      [ activeSkillSummaryId activeSkill
        | (_, BattleStateMsg snapshot) <- responses,
          activeSkill <- battleSnapshotActiveSkills snapshot
      ]

testProgressionActions :: IO ()
testProgressionActions = do
  gs <- newTestPlayerState
  (_, learned) <- runOk "learn from teacher" (withCombatNpcInStarterRoom gs) (playerLearnArt "tester" "weiyuan_training_dummy" "weiyuan_sword" 2)
  case M.lookup "tester" (learned ^. players) of
    Nothing -> fail "tester missing after teacher learning"
    Just player -> do
      assert (knowsArtAt "weiyuan_sword" 2 player) "teacher learning did not raise weiyuan_sword to level 2"
      assert (knowsArtAt "basic_sword" 2 player) "teacher learning did not sync foundation art"
      assert (preparedArtIn Sword "weiyuan_sword" player) "teacher learning did not prepare learned art"
      assert (enabledArtIn Sword "weiyuan_sword" player) "teacher learning did not enable learned art"
      assert ((player ^. playerPotential) == 18) "teacher learning did not consume potential"
      assert ((player ^. playerCharacter . charJing) == 96) "teacher learning did not consume jing"

  (_, researched) <- runOk "research learned art" learned (playerResearchArt "tester" "weiyuan_sword")
  case M.lookup "tester" (researched ^. players) of
    Nothing -> fail "tester missing after research"
    Just player -> do
      assert (knowsArtAt "weiyuan_sword" 3 player) "research did not improve the learned art"
      assert ((player ^. playerPotential) == 17) "research did not consume potential"
      assert ((player ^. playerCharacter . charJing) == 74) "research did not consume jing"

  (_, meditated) <- runOk "meditate for max qi" researched (playerMeditate "tester" 40)
  case M.lookup "tester" (meditated ^. players) of
    Nothing -> fail "tester missing after meditation"
    Just player -> do
      assert ((player ^. playerCharacter . charQi) == 60) "meditation did not consume qi"
      assert ((player ^. playerCharacter . charMaxQi) == 102) "meditation did not raise max qi"
      assert ((player ^. playerCharacter . charJing) == 69) "meditation did not consume jing"

testJingRequirementFailure :: IO ()
testJingRequirementFailure = do
  gs <- newTestPlayerState
  let exhausted =
        gs
          & players . ix "tester" . playerCharacter . charJing .~ 0
  result <- runGameState (withCombatNpcInStarterRoom exhausted) (playerLearnArt "tester" "weiyuan_training_dummy" "weiyuan_sword" 1)
  case result of
    Left (StructuredException (ErrorSummary code params)) -> do
      assert (code == "not_enough_jing") "jing failure used the wrong error code"
      assert (M.lookup "required" params == Just "12") "jing failure did not include the required amount"
      assert (M.lookup "current" params == Just "0") "jing failure did not include the current amount"
    Left err -> fail $ "expected structured jing error, got: " <> show err
    Right _ -> fail "learning succeeded without enough jing"

testJingTickRecovery :: IO ()
testJingTickRecovery = do
  gs <- newTestPlayerState
  let tired =
        gs
          & players . ix "tester" . playerCharacter . charJing .~ 10
  (_, recovered) <- runOk "recover jing tick" tired (onGameTick 1)
  case M.lookup "tester" (recovered ^. players) of
    Nothing -> fail "tester missing after jing recovery"
    Just player -> do
      assert ((player ^. playerCharacter . charJing) > 10) "jing did not recover on tick"
      assert ((player ^. playerCharacter . charJing) <= (deriveStats player ^. dsMaxJing)) "jing recovery exceeded derived max"

testLearningRequirementFailure :: IO ()
testLearningRequirementFailure = do
  gs <- newTestPlayerState
  let targetArtId = "weiyuan_sword"
      expectedArtName = gs ^. world . martialArts . ix targetArtId . artName
  let gated =
        gs
          & world . martialArts . ix targetArtId . artRequires .~ [ArtRequirement "basic_sword" 99]
  result <- runGameState gated (grantArt "tester" targetArtId 1)
  case result of
    Left (StructuredException (ErrorSummary code params)) -> do
      assert (code == "cannot_learn_art") "learning failure used the wrong error code"
      assert (M.lookup "art" params == Just expectedArtName) "learning failure did not include the art name"
      assert (maybe False ("基础剑法" `T.isInfixOf`) (M.lookup "requirements" params)) "learning failure did not include missing foundation"
    Left err -> fail $ "expected structured learning error, got: " <> show err
    Right _ -> fail "learning succeeded despite unmet foundation requirement"

testArtsQuery :: IO ()
testArtsQuery = do
  gs <- newTestPlayerState
  (responses, _) <- runOk "query arts" gs (playerArts "tester")
  case [arts | (_, ArtsMsg arts) <- responses] of
    [] -> fail "arts query did not send ArtsMsg"
    arts : _ -> do
      assert (any (\art -> artSummaryId art == "basic_sword" && artSummaryIsFoundation art) arts) "arts query omitted basic_sword foundation"
      assert (any (\art -> artSummaryId art == "nameless_trial_fist" && artSummaryType art == "fist") arts) "arts query omitted starter fist art"

testWeiyuanChapterFlow :: IO ()
testWeiyuanChapterFlow = do
  gs <- newTestPlayerState
  (warningResponses, warned) <- runOk "hear old escort warning" gs (playerTalk "tester" "wounded_escort")
  assert (questStageOf "weiyuan_bloody_case" warned == Just "intruder") "weiyuan quest did not enter intruder stage"
  assert (snd (head warningResponses) == StorySequenceMsg True) "story sequence did not lock output before the opening dialogue"
  assert (snd (last warningResponses) == StorySequenceMsg False) "story sequence did not unlock output after the opening dialogue"
  kickIndex <- requireResponseIndex "temple door narration" (== StoryMsg "旁白" "话音未落，庙门外响起急促脚步。半扇破门被人一脚踹开，雨水和冷风一起灌进殿里。") warningResponses
  revealIndex <- requireResponseIndex "black-clad room refresh" (\case ViewMsg _ _ characters _ -> any ((== "temple_black_clad") . roomCharacterSummaryId) characters; _ -> False) warningResponses
  threatIndex <- requireResponseIndex "black-clad threat" (== StoryMsg "黑衣人" "威远的人果然在这里。今夜这座庙里，不该再有活口。") warningResponses
  assert (kickIndex < revealIndex && revealIndex < threatIndex) "black-clad NPC was not revealed between the door narration and threat"

  directAttack <- runGameState warned (playerAttack "tester" "temple_black_clad")
  case directAttack of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected direct story NPC attack to be blocked, got: " <> show err
    Right _ -> fail "story NPC exposed a direct attack path"

  (battleStartResponses, inBattle) <- runOk "confront temple attacker" warned (playerTalk "tester" "temple_black_clad")
  assert (questStageOf "weiyuan_bloody_case" inBattle == Just "duel") "weiyuan quest did not enter duel stage"
  assert (S.member "tester" $ inBattle ^. isolatedBattles) "story battle was not marked isolated"
  assert (maybe False ((== CharAlive) . view charStatus) $ M.lookup "temple_black_clad" (inBattle ^. world . chars)) "story battle locked the global NPC"
  startedBattle <- getBattle inBattle
  assert (startedBattle ^. battleActionLockRemaining > 0) "story battle advanced while its opening dialogue was still queued"
  battleSnapshotIndex <- requireResponseIndex "visible story battle snapshot" (\case BattleStateMsg snapshot -> battleSnapshotActionLockRemaining snapshot == 0; _ -> False) battleStartResponses
  attackIndex <- requireResponseIndex "story battle attack message" (\case AttackMsg _ _ -> True; _ -> False) battleStartResponses
  assert (attackIndex < battleSnapshotIndex) "story battle snapshot arrived before its attack message"

  let defeated = inBattle & battles . ix "tester" . battleEnemyState . battleChar . charHP .~ 0
  (settlementResponses, afterFight) <- runOk "settle temple fight" defeated (updateBattle 0 "tester")
  assert (questStageOf "weiyuan_bloody_case" afterFight == Just "escort_dying") "weiyuan quest did not advance after the fight"
  assert (not $ S.member "tester" $ afterFight ^. isolatedBattles) "isolated battle marker was not cleared"
  assert (maybe False ((== CharAlive) . view charStatus) $ M.lookup "temple_black_clad" (afterFight ^. world . chars)) "isolated story fight changed the global NPC"
  settlementIndex <- requireResponseIndex "combat settlement" (\case CombatSettlementMsg _ _ _ -> True; _ -> False) settlementResponses
  aftermathDelayIndex <- requireResponseIndex "post-combat pause" (== StoryDelayMsg 1800) settlementResponses
  collapseIndex <- requireResponseIndex "post-combat collapse" (== StoryMsg "旁白" "黑衣人倒在门边。老镖师扶着断柱站起，才迈出半步，便又重重坐了下去。") settlementResponses
  growthRewardIndex <- requireResponseIndex "battle growth reward" (\case RewardMsg rewards -> any ((== "combat_exp") . rewardSummaryKind) rewards; _ -> False) settlementResponses
  assert (settlementIndex < aftermathDelayIndex && aftermathDelayIndex < collapseIndex && collapseIndex < growthRewardIndex) "battle aftermath was not paced after settlement or reward was shown too early"

  (roadResponses, onRoad) <- runOk "hear old escort last request" afterFight (playerTalk "tester" "wounded_escort")
  assert (questStageOf "weiyuan_bloody_case" onRoad == Just "escort_road") "weiyuan quest did not enter escort road stage"
  case M.lookup "tester" (onRoad ^. players) of
    Nothing -> fail "tester missing on escort road"
    Just player -> do
      assert ((player ^. playerPosition) == ("bianshui_road", (0, 0))) "old escort did not send the player to the escort road"
      assert ((player ^. playerMoney) >= 30) "sourced travel money was not granted"
  transitionIndex <- requireResponseIndex "temple exit transition" (== StoryTransitionMsg "你与少年冒雨离开破庙，踏上汴水官道。" 1800) roadResponses
  moveIndex <- requireResponseIndex "escort road move" (== MoveMsg "官道口") roadResponses
  roadDialogueIndex <- requireResponseIndex "escort road follow-up" (== StoryMsg "灰衣少年" "沿汴水往北先到南渡，过州桥才是城门。追兵未必只有一个，我们别在路上停。") roadResponses
  travelMoneyIndex <- requireResponseIndex "deferred travel money" (\case RewardMsg rewards -> any ((== "money") . rewardSummaryKind) rewards; _ -> False) roadResponses
  assert (transitionIndex < moveIndex && moveIndex < roadDialogueIndex && roadDialogueIndex < travelMoneyIndex) "transition, move, follow-up dialogue, and reward were not emitted in narrative order"

  (_, atFerry) <- runOk "escort young survivor into Kaifeng" onRoad (playerMove "tester" North)
  assert (questStageOf "weiyuan_bloody_case" atFerry == Just "branch_gate") "entering Kaifeng did not advance the escort"
  (_, atGate) <- runOk "approach Weiyuan branch" atFerry (playerMove "tester" East)
  (hallResponses, inHall) <- runOk "enter Weiyuan branch hall" atGate (playerMove "tester" East)
  assert (questStageOf "weiyuan_bloody_case" inHall == Just "completed") "weiyuan quest did not complete in the branch hall"
  assert (questStageOf "first_steps_kaifeng" inHall == Just "guide") "Kaifeng settling quest did not start"
  case M.lookup "tester" (inHall ^. stories) of
    Nothing -> fail "tester story state missing after Weiyuan separation"
    Just storyState -> do
      assert (S.member "weiyuan.youth_separated" $ storyState ^. storyFlags) "young survivor separation flag was not set"
      assert (S.member "grey_young_escort" $ storyState ^. storyHiddenNpcs) "young survivor was not hidden after separation"
  case M.lookup "tester" (inHall ^. players) of
    Nothing -> fail "tester missing after Weiyuan completion"
    Just player -> do
      assert ((player ^. playerInventory . at "soaked_route_note") == Just 1) "route note was not retained"
      assert ((player ^. playerInventory . at "weiyuan_sword_manual") == Just 1) "Weiyuan manual was not retained"
  farewellIndex <- requireResponseIndex "young escort farewell" (\case StoryMsg "灰衣少年" text -> "我从后院走" `T.isPrefixOf` text; _ -> False) hallResponses
  hideIndex <- requireResponseIndex "young escort hide refresh" (\case ViewMsg _ _ characters _ -> all ((/= "grey_young_escort") . roomCharacterSummaryId) characters; _ -> False) hallResponses
  itemRewardIndex <- requireResponseIndex "deferred clue rewards" (\case RewardMsg rewards -> any ((== Just "soaked_route_note") . rewardSummaryId) rewards; _ -> False) hallResponses
  assert (farewellIndex < hideIndex && hideIndex < itemRewardIndex) "young escort disappeared or clue rewards appeared before the farewell completed"

testStoryBattlesArePlayerIsolated :: IO ()
testStoryBattlesArePlayerIsolated = do
  gs <- newTestPlayerState
  (_, twoPlayers) <- runOk "create second story player" gs (createDefaultPlayer "tester2" "resources/scripts/default_player.yaml")
  (_, firstWarned) <- runOk "warn first player" twoPlayers (playerTalk "tester" "wounded_escort")
  (_, bothWarned) <- runOk "warn second player" firstWarned (playerTalk "tester2" "wounded_escort")
  (_, firstBattle) <- runOk "start first isolated story battle" bothWarned (playerTalk "tester" "temple_black_clad")
  (_, bothBattles) <- runOk "start second isolated story battle" firstBattle (playerTalk "tester2" "temple_black_clad")
  assert (M.member "tester" $ bothBattles ^. battles) "first isolated story battle disappeared"
  assert (M.member "tester2" $ bothBattles ^. battles) "second isolated story battle did not start"
  assert (maybe False ((== CharAlive) . view charStatus) $ M.lookup "temple_black_clad" (bothBattles ^. world . chars)) "concurrent story battles locked the shared NPC"

testKaifengSettlingAndTravelFlow :: IO ()
testKaifengSettlingAndTravelFlow = do
  gs <- newTestPlayerState
  let ready =
        gs
          & stories . ix "tester" . storyQuestStages . at "weiyuan_bloody_case" ?~ "completed"
          & stories . ix "tester" . storyQuestStages . at "first_steps_kaifeng" ?~ "guide"
  (_, atSouthGate) <- runOk "move to Kaifeng south gate" ready (movePlayerToRoom "tester" "kaifeng_city" (0, -1))
  (_, seekingInn) <- runOk "ask guide for lodging" atSouthGate (playerTalk "tester" "kaifeng_guide")
  assert (questStageOf "first_steps_kaifeng" seekingInn == Just "inn") "guide did not send the player to the inn"

  (_, atInn) <- runOk "move to Fanlou inn" seekingInn (movePlayerToRoom "tester" "kaifeng_city" (-1, 1))
  (_, lodged) <- runOk "lodge at Fanlou" atInn (playerTalk "tester" "fanlou_innkeeper")
  assert (questStageOf "first_steps_kaifeng" lodged == Just "training") "lodging did not advance to training"
  case M.lookup "tester" (lodged ^. players) of
    Nothing -> fail "tester missing after lodging"
    Just player -> assert ((player ^. playerInventory . at "kaifeng_room_tag") == Just 1) "inn did not hand over the room tag"

  (_, atTraining) <- runOk "move to training yard" lodged (movePlayerToRoom "tester" "kaifeng_city" (-1, -1))
  (_, readyToPractice) <- runOk "speak with Liang instructor" atTraining (playerTalk "tester" "kaifeng_martial_instructor")
  assert (questStageOf "first_steps_kaifeng" readyToPractice == Just "practice") "instructor did not open the practice fight"
  (_, practiceBattle) <- runOk "start practice fight" readyToPractice (playerTalk "tester" "kaifeng_training_dummy")
  let defeatedDummy = practiceBattle & battles . ix "tester" . battleEnemyState . battleChar . charHP .~ 0
  (_, afterPractice) <- runOk "settle practice fight" defeatedDummy (updateBattle 0 "tester")
  assert (questStageOf "first_steps_kaifeng" afterPractice == Just "first_job") "practice fight did not unlock the first job"

  (_, atScribe) <- runOk "move to scribe" afterPractice (movePlayerToRoom "tester" "kaifeng_city" (1, -1))
  (_, settled) <- runOk "complete first city job" atScribe (playerTalk "tester" "old_scribe")
  assert (questStageOf "first_steps_kaifeng" settled == Just "completed") "first city job did not complete Kaifeng settling"
  case M.lookup "tester" (settled ^. players) of
    Nothing -> fail "tester missing after first city job"
    Just player -> assert ((player ^. playerInventory . at "kaifeng_work_token") == Just 1) "employer did not hand over the work token"

  (_, backAtGuide) <- runOk "return to guide" settled (movePlayerToRoom "tester" "kaifeng_city" (0, -1))
  (_, choosingRoute) <- runOk "ask guide about travel" backAtGuide (playerTalk "tester" "kaifeng_guide")
  assert (questStageOf "three_city_leads" choosingRoute == Just "choose_route") "guide did not open the travel choice"
  (_, atLuoyangRunner) <- runOk "move to Luoyang runner" choosingRoute (movePlayerToRoom "tester" "kaifeng_city" (-2, 2))
  (_, inLuoyang) <- runOk "choose Luoyang as first journey" atLuoyangRunner (playerTalk "tester" "luoyang_runner")
  assert (questStageOf "three_city_leads" inLuoyang == Just "completed") "choosing one route did not complete the travel introduction"
  case M.lookup "tester" (inLuoyang ^. players) of
    Nothing -> fail "tester missing after travel"
    Just player -> do
      assert ((player ^. playerPosition) == ("luoyang_city", (0, 0))) "travel contact did not move the player to Luoyang"
      assert ((player ^. playerInventory . at "three_city_route_pass") == Just 1) "travel contact did not hand over the route pass"

testPlayerSaveRoundTrip :: IO ()
testPlayerSaveRoundTrip = do
  gs <- newTestPlayerState
  let fixtureQuestId = "fixture_quest"
      accepted =
        gs
          & stories . ix "tester" . storyQuestStages . at fixtureQuestId ?~ "accepted"
  let rewarded =
        accepted
          & players . ix "tester" . playerPosition .~ ("kaifeng_city", (0, 1))
          & players . ix "tester" . playerMoney .~ 80
          & players . ix "tester" . playerPotential .~ 12
          & players . ix "tester" . playerCombatExp .~ 345
          & players . ix "tester" . playerInventory . at "saved_token" .~ Just 1
          & players . ix "tester" . playerInventory . at "saved_manual" .~ Just 1
          & players . ix "tester" . playerCharacter . charQi .~ 72
          & players . ix "tester" . playerCharacter . charMaxQi .~ 123
          & players . ix "tester" . playerCharacter . charJing .~ 91
          & players . ix "tester" . playerCharacter . charDesc .~ "我从汴梁来。"
          & players . ix "tester" . playerCharacter . charGender .~ Female
          & players . ix "tester" . playerCharacter . charAppearance .~ 8
          & players . ix "tester" . playerCharacter . charInnate .~ InnateAttrs 21 17 16
          & players . ix "tester" . playerCharacter . charArt . at Foundation .~ Just [ArtEntity "basic_sword" 5 0]
          & players . ix "tester" . playerCharacter . charArt . at Sword .~ Just [ArtEntity "saved_sword_art" 5 0]
          & players . ix "tester" . playerCharacter . charPrepare . at Sword .~ Just (ArtEntity "saved_sword_art" 5 0)
          & players . ix "tester" . playerCharacter . charEnabled . at Sword .~ Just (ArtEntity "saved_sword_art" 5 0)
  savePlayerState ".stack-work/test-saves" "tester" rewarded
  saveResult <- loadPlayerSave ".stack-work/test-saves" "tester"
  save <- case saveResult of
    Left err -> fail $ "failed to load player save: " <> T.unpack err
    Right Nothing -> fail "player save was not written"
    Right (Just loaded) -> pure loaded
  assert (saveVersion save == 6) "player save version was not bumped"
  fresh <- newTestPlayerState
  let restored = applyPlayerSaveToGameState save fresh
  assert (questStageOf fixtureQuestId restored == Just "accepted") "saved quest stage was not restored"
  case M.lookup "tester" (restored ^. players) of
    Nothing -> fail "tester missing after save restore"
    Just player -> do
      assert ((player ^. playerMoney) == 80) "saved money was not restored"
      assert ((player ^. playerPosition) == ("kaifeng_city", (0, 1))) "saved player position was not restored"
      assert ((player ^. playerPotential) == 12) "saved potential was not restored"
      assert ((player ^. playerCombatExp) == 345) "saved combat exp was not restored"
      assert ((player ^. playerInventory . at "saved_token") == Just 1) "saved inventory was not restored"
      assert ((player ^. playerInventory . at "saved_manual") == Just 1) "saved manual inventory was not restored"
      assert ((player ^. playerCharacter . charQi) == 72) "saved qi was not restored"
      assert ((player ^. playerCharacter . charMaxQi) == 123) "saved max qi was not restored"
      assert ((player ^. playerCharacter . charJing) == 91) "saved jing was not restored"
      assert ((player ^. playerCharacter . charDesc) == "我从汴梁来。") "saved desc was not restored"
      assert ((player ^. playerCharacter . charGender) == Female) "saved gender was not restored"
      assert ((player ^. playerCharacter . charAppearance) == 8) "saved appearance was not restored"
      assert ((player ^. playerCharacter . charInnate) == InnateAttrs 21 17 16) "saved innate attrs were not restored"
      assert ((player ^. playerCharacter . charArt . at Foundation) == Just [ArtEntity "basic_sword" 5 0]) "saved foundation art was not restored"
      assert ((player ^. playerCharacter . charArt . at Sword) == Just [ArtEntity "saved_sword_art" 5 0]) "saved learned martial art was not restored"
      assert ((player ^. playerCharacter . charPrepare . at Sword) == Just (ArtEntity "saved_sword_art" 5 0)) "saved prepared martial art was not restored"
      assert ((player ^. playerCharacter . charEnabled . at Sword) == Just (ArtEntity "saved_sword_art" 5 0)) "saved enabled martial art was not restored"
      assert (S.member "tester" $ roomPlayersAtMap "kaifeng_city" restored (0, 1)) "restored room occupancy did not include the player"
      assert (not $ S.member "tester" $ roomPlayersAt restored (3, 3)) "default room occupancy still contained the restored player"
  let legacyRestored = applyPlayerSaveToGameState (save {saveVersion = 5}) fresh
  assert (questStageOf fixtureQuestId legacyRestored == Nothing) "pre-migration story ids were restored"
  case M.lookup "tester" (legacyRestored ^. players) of
    Nothing -> fail "tester missing after legacy save restore"
    Just player ->
      assert ((player ^. playerPosition) == ("weiyuan_road", (3, 3))) "pre-migration position was restored"
