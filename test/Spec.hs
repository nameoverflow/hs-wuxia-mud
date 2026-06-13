{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad (replicateM_, unless)
import Control.Monad.Random (mkStdGen, runRand)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Database
import Game.Combat
import Game.Entity
import Game.Message
import Game.Quest
import Game.World
import GamePlay
import GameState
import Utils

main :: IO ()
main = do
  testItemsLoad
  testEffectsLoad
  testQuestLoad
  testWorldValidationCatchesBrokenQuestRefs
  testRandomSelectEmpty
  testDefaultFoundationArts
  testMoveUpdatesRoomOccupancy
  testCannotAttackAcrossRooms
  testDefeatDoesNotKillNpc
  testActiveSkillFailureIsSpecific
  testActiveSkillConsumesApAndSendsSnapshot
  testDotEffectTicks
  testTrainRaisesFoundationAndUnlocksActiveSkills
  testLearningRequirementFailure
  testArtsQuery
  testColdRainChapterFlow
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

newTestPlayerState :: IO GameState
newTestPlayerState = do
  gs <- loadFreshState
  snd <$> runOk "create default player" gs (createDefaultPlayer "tester" "resources/scripts/default_player.yaml")

roomPlayersAt :: GameState -> (Int, Int) -> S.Set PlayerId
roomPlayersAt gs pos =
  case M.lookup "test_map" (gs ^. world . maps) >>= M.lookup pos . view mapRooms of
    Nothing -> S.empty
    Just room -> room ^. roomPlayer

getNpc :: GameState -> IO Character
getNpc gs =
  case M.lookup "char_in_test" (gs ^. world . chars) of
    Nothing -> fail "char_in_test missing"
    Just npc -> pure npc

getBattle :: GameState -> IO Battle
getBattle gs =
  case M.lookup "tester" (gs ^. battles) of
    Nothing -> fail "tester battle missing"
    Just battle -> pure battle

startTrainingBattle :: GameState -> IO ([PlayerResp], GameState)
startTrainingBattle gs = do
  (_, atWoodshed) <- runOk "move west to training dummy" gs (playerMove "tester" West)
  runOk "start training battle" atWoodshed (playerAttack "tester" "char_in_test")

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

testEffectsLoad :: IO ()
testEffectsLoad = do
  gs <- loadFreshState
  assert (M.size (gs ^. world . effects) == 5) "effect definitions were not loaded"

testItemsLoad :: IO ()
testItemsLoad = do
  gs <- loadFreshState
  assert (M.member "cold_rain_token" (gs ^. world . items)) "cold_rain_token item was not loaded"
  assert (M.member "cold_rain_manual" (gs ^. world . items)) "cold_rain_manual item was not loaded"
  case M.lookup "cold_rain_manual" (gs ^. world . items) >>= view itemUse of
    Just (LearnArtUse "cold_rain_secret" 1 False _ _) -> pure ()
    _ -> fail "cold_rain_manual did not load the configured learn_art use effect"

testQuestLoad :: IO ()
testQuestLoad = do
  gs <- loadFreshState
  assert (M.member "cold_rain_inn" (gs ^. world . quests)) "cold_rain_inn quest was not loaded"

testWorldValidationCatchesBrokenQuestRefs :: IO ()
testWorldValidationCatchesBrokenQuestRefs = do
  gs <- loadFreshState
  let broken =
        gs
          ^. world
          & quests . ix "cold_rain_inn" . questEvents . ix 0 . questEventActions %~ (++ [StartBattle "missing_npc"])
  case validateWorld broken of
    Left err ->
      assert ("missing_npc" `T.isInfixOf` err) "world validation error did not identify the missing NPC"
    Right _ -> fail "world validation accepted a quest with a missing NPC reference"

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
      assert (preparedArtIn Fist "nameless_trial_fist" player) "default player did not prepare the starter fist art"
      assert ((player ^. playerCharacter . charPrepare . at Foundation) == Nothing) "foundation art should not be prepared"

testMoveUpdatesRoomOccupancy :: IO ()
testMoveUpdatesRoomOccupancy = do
  gs <- newTestPlayerState
  (_, moved) <- runOk "move north" gs (playerMove "tester" North)
  assert (not $ S.member "tester" (roomPlayersAt moved (3, 3))) "player remained in the old room after moving"
  assert (S.member "tester" (roomPlayersAt moved (3, 4))) "player was not added to the new room after moving"

testCannotAttackAcrossRooms :: IO ()
testCannotAttackAcrossRooms = do
  gs <- newTestPlayerState
  (_, moved) <- runOk "move north" gs (playerMove "tester" North)
  result <- runGameState moved (playerAttack "tester" "char_in_test")
  case result of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected UnableToInteract Attacking, got: " <> show err
    Right _ -> fail "player attacked an NPC from a different room"

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
  assert (M.notMember "char_in_test" (settled ^. respawn)) "NPC respawn was scheduled when the player lost"
  case M.lookup "tester" (settled ^. players) of
    Nothing -> fail "tester missing after defeat"
    Just player -> do
      assert ((player ^. playerStatus) == PlayerNormal) "player was not returned to normal status after defeat"
      assert ((player ^. playerCharacter . charHP) == 1) "defeated player should be left at 1 HP"

testActiveSkillFailureIsSpecific :: IO ()
testActiveSkillFailureIsSpecific = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  (responses, _) <- runOk "perform active skill without AP" inBattle (playerPerformActiveSkill "tester" "power_strike")
  assert
    (any (\(_, resp) -> resp == ActiveSkillFailureMsg (ActiveSkillNeedAp 60 0)) responses)
    "active skill failure did not report the specific AP requirement"

testActiveSkillConsumesApAndSendsSnapshot :: IO ()
testActiveSkillConsumesApAndSendsSnapshot = do
  gs <- newTestPlayerState
  (_, inBattle) <- startTrainingBattle gs
  let ready =
        inBattle
          & battles . ix "tester" . battleState . battleAp .~ 60
  (responses, afterSkill) <- runOk "perform power strike" ready (playerPerformActiveSkill "tester" "power_strike")
  battle <- getBattle afterSkill
  assert ((battle ^. battleState . battleAp) == 0) "active skill did not consume AP"
  assert ((battle ^. battleState . battleQi) == 70) "active skill did not consume Qi"
  assert ((battle ^. battleEnemyState . battleChar . charHP) == 79) "active skill did not damage the enemy"
  assert (any (isBattleStateMsg . snd) responses) "active skill success did not send a battle snapshot"
  where
    isBattleStateMsg (BattleStateMsg _) = True
    isBattleStateMsg _ = False

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
  (_, learned) <- runOk "learn cold rain secret" gs (grantArt "tester" "cold_rain_secret" 1)

  (lowBattleMsgs, _) <- startTrainingBattle learned
  let lowActiveSkillIds = battleActiveSkillIds lowBattleMsgs
  assert ("lamp_cut" `elem` lowActiveSkillIds) "level 1 cold_rain_secret did not expose lamp_cut"
  assert ("umbrella_spine_eight" `notElem` lowActiveSkillIds) "umbrella_spine_eight unlocked before level 5"

  (_, trained) <- runOk "train cold rain secret to 5" learned $
    replicateM_ 4 (playerTrainArt "tester" "cold_rain_secret")
  case M.lookup "tester" (trained ^. players) of
    Nothing -> fail "tester missing after training"
    Just player -> do
      assert (knowsArtAt "cold_rain_secret" 5 player) "training did not raise cold_rain_secret to level 5"
      assert (knowsArtAt "basic_sword" 5 player) "training did not raise basic_sword to level 5"
      assert (preparedArtIn Sword "cold_rain_secret" player) "training did not keep cold_rain_secret prepared"

  (highBattleMsgs, _) <- startTrainingBattle trained
  let highActiveSkillIds = battleActiveSkillIds highBattleMsgs
  assert ("umbrella_spine_eight" `elem` highActiveSkillIds) "umbrella_spine_eight did not unlock at level 5"
  where
    battleActiveSkillIds responses =
      [ activeSkillSummaryId activeSkill
        | (_, BattleStateMsg snapshot) <- responses,
          activeSkill <- battleSnapshotActiveSkills snapshot
      ]

testLearningRequirementFailure :: IO ()
testLearningRequirementFailure = do
  gs <- newTestPlayerState
  let gated =
        gs
          & world . martialArts . ix "cold_rain_secret" . artRequires .~ [ArtRequirement "basic_sword" 99]
  result <- runGameState gated (grantArt "tester" "cold_rain_secret" 1)
  case result of
    Left (StructuredException (ErrorSummary code params)) -> do
      assert (code == "cannot_learn_art") "learning failure used the wrong error code"
      assert (M.lookup "art" params == Just "听雨残谱") "learning failure did not include the art name"
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

testColdRainChapterFlow :: IO ()
testColdRainChapterFlow = do
  gs <- newTestPlayerState
  (acceptedMsgs, accepted) <- runOk "talk to innkeeper" gs (playerTalk "tester" "cold_rain_innkeeper")
  assert (any (storyTextContains "这杯酒已经冷了三次" . snd) acceptedMsgs) "intro story text was not emitted"
  assert (any (storyTextContains "酒杯入手很轻" . snd) acceptedMsgs) "intro follow-up story text was not emitted"
  assert (questStageOf "cold_rain_inn" accepted == Just "accepted") "quest did not enter accepted stage"
  assert (any (questObjective "把冷酒送到北面的客栈大堂" . snd) acceptedMsgs) "accepted stage did not update quest objective"

  (_, inHall) <- runOk "move to hall" accepted (playerMove "tester" North)
  (qingyi, witness) <- runOk "talk to qingyi guest" inHall (playerTalk "tester" "qingyi_guest")
  assert (any (storyTextContains "酒不是给我喝的" . snd) qingyi) "qingyi story text was not emitted"
  assert (any (storyTextContains "青衣客没有碰那杯酒" . snd) qingyi) "qingyi follow-up story text was not emitted"
  assert (questStageOf "cold_rain_inn" witness == Just "witness") "quest did not enter witness stage"

  (courtyardMsgs, inCourtyard) <- runOk "enter courtyard" witness (playerMove "tester" East)
  assert (any (storyTextContains "伞下的人" . snd) courtyardMsgs) "courtyard reveal did not fire"

  (killerTalk, inStoryBattle) <- runOk "talk to paper umbrella killer" inCourtyard (playerTalk "tester" "paper_umbrella_killer")
  assert (any (storyTextContains "旁观者最安全" . snd) killerTalk) "killer story text was not emitted"
  assert (any (storyTextContains "灯笼落地" . snd) killerTalk) "killer follow-up story text was not emitted"
  assert (M.member "tester" (inStoryBattle ^. battles)) "story battle did not start"

  let defeated =
        inStoryBattle
          & battles . ix "tester" . battleEnemyState . battleChar . charHP .~ 0
  (ending, resolved) <- runOk "settle story battle" defeated (updateBattle 0 "tester")
  assert (questStageOf "cold_rain_inn" resolved == Just "completed") "chapter did not complete after the story battle"
  assert (any (storyTextContains "纸伞落在井边" . snd) ending) "chapter ending message was not emitted"
  assert (any (rewardMoney 80 . snd) ending) "chapter reward money was not emitted"
  assert (any (rewardItem "cold_rain_token" 1 . snd) ending) "story kill drop reward was not emitted"
  assert (any (inventoryHas "cold_rain_token" 1 . snd) ending) "chapter reward inventory snapshot was not emitted"
  assert
    (maybe False (S.member "paper_umbrella_killer" . view storyHiddenNpcs) (M.lookup "tester" (resolved ^. stories)))
    "defeated story NPC was not hidden for the player"
  case M.lookup "tester" (resolved ^. players) of
    Nothing -> fail "tester missing after story resolution"
    Just player -> do
      assert ((player ^. playerMoney) == 80) "chapter reward money was not applied"
      assert ((player ^. playerInventory . at "cold_rain_token") == Just 1) "chapter reward item was not applied"

  let respawnedForTester =
        resolved
          & world . chars . ix "paper_umbrella_killer" . charStatus .~ CharAlive
  repeatAttack <- runGameState respawnedForTester (playerAttack "tester" "paper_umbrella_killer")
  case repeatAttack of
    Left (UnableToInteract _ Attacking) -> pure ()
    Left err -> fail $ "expected hidden NPC attack to be blocked, got: " <> show err
    Right _ -> fail "completed story NPC could be attacked after respawn"
  (viewAfterCompletion, _) <- runOk "view after story completion" respawnedForTester (playerView "tester")
  case [visibleChars | (_, ViewMsg _ _ visibleChars _) <- viewAfterCompletion] of
    [] -> fail "view after completion did not send a room view"
    visibleChars : _ ->
      assert
        (all ((/= "paper_umbrella_killer") . roomCharacterSummaryId) visibleChars)
        "hidden story NPC was still visible in the room view"

  (_, inHallAfterCompletion) <- runOk "return to hall after story completion" resolved (playerMove "tester" West)
  (hallView, hallAfterView) <- runOk "view hall after story completion" inHallAfterCompletion (playerView "tester")
  case [visibleChars | (_, ViewMsg _ _ visibleChars _) <- hallView] of
    [] -> fail "hall view after completion did not send a room view"
    visibleChars : _ ->
      case filter ((== "qingyi_guest") . roomCharacterSummaryId) visibleChars of
        [qingyiSummary] -> do
          assert ("talk" `elem` roomCharacterSummaryActions qingyiSummary) "qingyi guest should remain talkable after completion"
          assert ("attack" `notElem` roomCharacterSummaryActions qingyiSummary) "qingyi guest should not expose an attack action"
        _ -> fail "qingyi guest was not visible exactly once after story completion"
  (epilogue, afterManual) <- runOk "talk to qingyi after story completion" hallAfterView (playerTalk "tester" "qingyi_guest")
  assert (any (storyTextContains "最难还的不是仇" . snd) epilogue) "qingyi guest did not keep the completion epilogue dialogue"
  assert (any (rewardItem "cold_rain_manual" 1 . snd) epilogue) "qingyi guest did not grant the manual item"
  assert (not $ any (rewardArt "cold_rain_secret" . snd) epilogue) "qingyi guest should not teach the martial art before the manual is used"
  assert (any (inventoryHas "cold_rain_manual" 1 . snd) epilogue) "manual reward inventory snapshot was not emitted"
  assert (any (inventoryUsable "cold_rain_manual" . snd) epilogue) "manual reward inventory snapshot did not mark the manual usable"
  case M.lookup "tester" (afterManual ^. players) of
    Nothing -> fail "tester missing after manual reward"
    Just player -> do
      assert ((player ^. playerInventory . at "cold_rain_manual") == Just 1) "manual reward item was not applied"
      assert (not $ knowsArtAt "cold_rain_secret" 1 player) "manual reward taught the martial art before use"
      assert (not $ preparedArtIn Sword "cold_rain_secret" player) "manual reward prepared the martial art before use"

  (manualUse, afterManualUse) <- runOk "use cold rain manual" afterManual (processPlayerAction "tester" (Use "cold_rain_manual"))
  assert (any (useItemTextContains "先听雨" . snd) manualUse) "manual use did not emit the configured use message"
  assert (any (rewardArt "cold_rain_secret" . snd) manualUse) "using the manual did not grant the martial art"
  assert (any (inventoryHas "cold_rain_manual" 1 . snd) manualUse) "manual should remain in inventory after use"
  case M.lookup "tester" (afterManualUse ^. players) of
    Nothing -> fail "tester missing after manual use"
    Just player -> do
      assert ((player ^. playerInventory . at "cold_rain_manual") == Just 1) "manual use changed the manual count"
      assert (knowsArtAt "cold_rain_secret" 1 player) "manual use did not teach the martial art"
      assert (preparedArtIn Sword "cold_rain_secret" player) "manual use did not prepare the learned martial art"

  (repeatUse, afterRepeatUse) <- runOk "use cold rain manual again" afterManualUse (processPlayerAction "tester" (Use "cold_rain_manual"))
  assert (any (useItemTextContains "又翻了一遍" . snd) repeatUse) "repeated manual use did not emit the configured repeat message"
  assert (not $ any (rewardArt "cold_rain_secret" . snd) repeatUse) "repeated manual use granted the martial art again"
  case M.lookup "tester" (afterRepeatUse ^. players) of
    Nothing -> fail "tester missing after repeated manual use"
    Just player ->
      assert ((player ^. playerInventory . at "cold_rain_manual") == Just 1) "manual count changed after repeated use"

  (repeatEpilogue, afterRepeatManual) <- runOk "talk to qingyi after manual reward" afterRepeatUse (playerTalk "tester" "qingyi_guest")
  assert (not $ any (rewardItem "cold_rain_manual" 1 . snd) repeatEpilogue) "manual reward was granted more than once"
  assert (not $ any (rewardArt "cold_rain_secret" . snd) repeatEpilogue) "martial art reward was granted more than once"
  case M.lookup "tester" (afterRepeatManual ^. players) of
    Nothing -> fail "tester missing after repeated manual dialogue"
    Just player ->
      assert ((player ^. playerInventory . at "cold_rain_manual") == Just 1) "manual count changed after repeated dialogue"
  where
    storyTextContains expected (StoryMsg _ text) = expected `T.isInfixOf` text
    storyTextContains _ _ = False

    questObjective expected (QuestLogMsg entries) =
      any (\entry -> maybe False (expected `T.isInfixOf`) (questLogEntryObjective entry)) entries
    questObjective _ _ = False

    rewardMoney expected (RewardMsg rewards) =
      any (\reward -> rewardSummaryKind reward == "money" && rewardSummaryAmount reward == expected) rewards
    rewardMoney _ _ = False

    rewardItem expectedId expectedAmount (RewardMsg rewards) =
      any
        ( \reward ->
            rewardSummaryKind reward == "item"
              && rewardSummaryId reward == Just expectedId
              && rewardSummaryAmount reward == expectedAmount
        )
        rewards
    rewardItem _ _ _ = False

    rewardArt expectedId (RewardMsg rewards) =
      any
        ( \reward ->
            rewardSummaryKind reward == "martial_art"
              && rewardSummaryId reward == Just expectedId
        )
        rewards
    rewardArt _ _ = False

    inventoryHas expectedId expectedAmount (InventoryMsg _ invItems) =
      any
        ( \invEntry ->
            inventoryItemSummaryId invEntry == expectedId
              && inventoryItemSummaryAmount invEntry == expectedAmount
        )
        invItems
    inventoryHas _ _ _ = False

    inventoryUsable expectedId (InventoryMsg _ invItems) =
      any
        ( \invEntry ->
            inventoryItemSummaryId invEntry == expectedId
              && inventoryItemSummaryUsable invEntry
        )
        invItems
    inventoryUsable _ _ = False

    useItemTextContains expected (UseItemMsg _ text) = expected `T.isInfixOf` text
    useItemTextContains _ _ = False

testPlayerSaveRoundTrip :: IO ()
testPlayerSaveRoundTrip = do
  gs <- newTestPlayerState
  (_, accepted) <- runOk "talk to innkeeper before save" gs (playerTalk "tester" "cold_rain_innkeeper")
  let rewarded =
        accepted
          & players . ix "tester" . playerMoney .~ 80
          & players . ix "tester" . playerInventory . at "cold_rain_token" .~ Just 1
          & players . ix "tester" . playerInventory . at "cold_rain_manual" .~ Just 1
          & players . ix "tester" . playerCharacter . charArt . at Foundation .~ Just [ArtEntity "basic_sword" 5]
          & players . ix "tester" . playerCharacter . charArt . at Sword .~ Just [ArtEntity "cold_rain_secret" 5]
          & players . ix "tester" . playerCharacter . charPrepare . at Sword .~ Just (ArtEntity "cold_rain_secret" 5)
  savePlayerState ".stack-work/test-saves" "tester" rewarded
  saveResult <- loadPlayerSave ".stack-work/test-saves" "tester"
  save <- case saveResult of
    Left err -> fail $ "failed to load player save: " <> T.unpack err
    Right Nothing -> fail "player save was not written"
    Right (Just loaded) -> pure loaded
  fresh <- newTestPlayerState
  let restored = applyPlayerSaveToGameState save fresh
  assert (questStageOf "cold_rain_inn" restored == Just "accepted") "saved quest stage was not restored"
  case M.lookup "tester" (restored ^. players) of
    Nothing -> fail "tester missing after save restore"
    Just player -> do
      assert ((player ^. playerMoney) == 80) "saved money was not restored"
      assert ((player ^. playerInventory . at "cold_rain_token") == Just 1) "saved inventory was not restored"
      assert ((player ^. playerInventory . at "cold_rain_manual") == Just 1) "saved manual inventory was not restored"
      assert ((player ^. playerCharacter . charArt . at Foundation) == Just [ArtEntity "basic_sword" 5]) "saved foundation art was not restored"
      assert ((player ^. playerCharacter . charArt . at Sword) == Just [ArtEntity "cold_rain_secret" 5]) "saved learned martial art was not restored"
      assert ((player ^. playerCharacter . charPrepare . at Sword) == Just (ArtEntity "cold_rain_secret" 5)) "saved prepared martial art was not restored"
