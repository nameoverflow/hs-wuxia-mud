{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GamePlay where

import Control.Lens
import Control.Monad (forM_, when)
import qualified Control.Monad
import Control.Monad.Except
  ( MonadError (throwError),
    unless,
  )
import Control.Monad.RWS (tell)
import Control.Monad.Random (getStdGen, MonadIO (liftIO), newStdGen, runRand)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Game.Combat
import Game.Entity
import Game.Message
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
  _ -> return ()

processBattleAction :: PlayerId -> PlayerAction -> GameStateT ()
processBattleAction pid action = case action of
  -- Perform skillId -> do
  --   player <- getsPlayer pid
  --   battle <- getsBattle pid
  --   let (battle', skill) = runState (useSkill skillId) battle
  --   reuturn ()

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
      players . at pid . _Just . playerPosition .= (curMapId, dst)
      -- broadcastMessage $ T.concat [playerId, " has entered ", newRoom ^. roomName]
      world . maps . ix curMapId . mapRooms . ix dst . roomPlayer %= S.insert pid
      newRoom <- liftWorld $ getsMapRoom curMapId dst
      tell [(pid, MoveMsg $ newRoom ^. roomName)]
      playerView pid
      return ()


playerView :: PlayerId -> GameStateT ()
playerView pid = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer pid
  curRoom <- liftWorld $ getsMapRoom curMapId curPos
  allChars <- use $ world . chars
  let roomChar' = curRoom ^. roomChar
  let curRoomChars = catMaybes $ flip map roomChar' $ \cid ->
        case M.lookup cid allChars of
          Nothing -> Nothing
          Just char ->
            if char ^. charStatus == CharAlive
              then Just $ char ^. charName
              else Nothing
  let curRoomExits = M.keys $ curRoom ^. roomExits
  let curRoomName = curRoom ^. roomName
  let curRoomDesc = curRoom ^. roomDesc
  tell [(pid, ViewMsg curRoomName curRoomDesc curRoomChars curRoomExits)]

playerTalk :: PlayerId -> CharId -> GameStateT ()
playerTalk pid target = do
  player <- getsPlayer pid
  npc <- liftWorld $ getsCharacter target
  unless (charTalkable npc) $ throwError $ UnableToInteract npc Dialogue

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
  npc <- liftWorld $ getsCharacter target
  unless (charAttackable npc) $ throwError $ UnableToInteract npc Attacking
  battles . at pid .= Just (newBattle player npc)
  players . ix pid . playerStatus .= PlayerInBattle
  tell [(pid, AttackMsg (player ^. playerCharacter . charName) (npc ^. charName))]
  where
    charAttackable :: Character -> Bool
    charAttackable char = isJust (char ^. charActions . at Attacking) && char ^. charStatus == CharAlive

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
updateBattle :: Double -> BattleId -> GameStateT ()
updateBattle dt bId = do
  wrld <- use world
  battle <- getsBattle bId
  -- Update battle state
  randG <- newStdGen
  (battleOver, battle', bttlMsg) <- runCombat randG ExceptionInCombat wrld battle $ flushBattleTick dt
  tell bttlMsg
  if battleOver
    then do
      liftIO $ TIO.putStrLn "Battle over"
      battleSettlement (battle' ^. battleState . battleChar . charHP <= 0) battle'
    else do
      -- Update battle state
      battles . at bId .= Just battle'

battleSettlement :: Bool -> Battle -> GameStateT ()
battleSettlement win battle = do
  let pChar = battle ^. battleState . battleChar
  let eChar = battle ^. battleEnemyState . battleChar
  let player = battle ^. battleOwner

  -- update status
  battles . at player .= Nothing
  players . ix player . playerStatus .= PlayerNormal
  world . chars . ix (eChar ^. charId) . charStatus .= CharDead

  -- set respawn time
  respawn . at (eChar ^. charId) .= Just (fromIntegral (eChar ^. charRespawn))

  -- TODO: battle rewards

  -- send message
  tell [(player, CombatSettlementMsg player (eChar ^. charName) win)]
  return ()
