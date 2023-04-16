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
import Control.Monad.Random (getStdGen, MonadIO (liftIO))
import qualified Data.Map as M
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
import Debug.Trace (trace)

-- import Script

processPlayerNormalAction :: T.Text -> PlayerAction -> GameStateT ()
processPlayerNormalAction curPlayerId action = case action of
  Go direction -> playerMove curPlayerId direction
  Attack (target :: CharId) -> playerAttack curPlayerId target
  _ -> return ()

processPlayerBattleAction :: T.Text -> PlayerAction -> GameStateT ()
processPlayerBattleAction curPlayerId action = case action of
  -- ApplySkill skillId -> do
  --   player <- getsPlayer curPlayerId
  --   battle <- getsBattle curPlayerId
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
  _ -> pure ()

playerMove :: T.Text -> Direction -> GameStateT ()
playerMove curPlayerId direction = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer curPlayerId
  currentRoom <- liftWorld $ getsMapRoom curMapId curPos
  case currentRoom ^. roomExits . at direction of
    Nothing -> throwError $ UnableToMove direction currentRoom
    Just dst -> do
      players . at curPlayerId . _Just . playerPosition .= (curMapId, dst)
      -- broadcastMessage $ T.concat [playerId, " has entered ", newRoom ^. roomName]
      world . maps . ix curMapId . mapRooms . ix dst . roomPlayer %= S.insert curPlayerId
      newRoom <- liftWorld $ getsMapRoom curMapId dst
      tell [(curPlayerId, MoveMsg $ newRoom ^. roomName)]
      playerView curPlayerId
      return ()


playerView :: T.Text -> GameStateT ()
playerView curPlayerId = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer curPlayerId
  curRoom <- liftWorld $ getsMapRoom curMapId curPos
  allChars <- use $ world . chars
  let curRoomChars = catMaybes $ flip map (curRoom ^. roomChar) $ \cid ->
        case M.lookup cid allChars of
          Nothing -> Nothing
          Just char -> Just $ char ^. charName
  let curRoomExits = M.keys $ curRoom ^. roomExits
  let curRoomName = curRoom ^. roomName
  let curRoomDesc = curRoom ^. roomDesc
  tell [(curPlayerId, ViewMsg curRoomName curRoomDesc curRoomChars curRoomExits)]

-- Create a new battle between the player and the target NPC
playerAttack :: PlayerId -> CharId -> GameStateT ()
playerAttack curPlayerId target = do
  player <- getsPlayer curPlayerId
  npc <- liftWorld $ getsCharacter target
  unless (charAttackable npc) $ throwError $ UnableToAttack npc
  battles . at curPlayerId .= Just (newBattle player npc)
  b' <- use battles
  trace (show b') $ pure ()
  players . ix curPlayerId . playerStatus .= PlayerInBattle
  tell [(curPlayerId, AttackMsg (player ^. playerCharacter . charName) (npc ^. charName))]
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
  -- liftIO $ TIO.putStrLn $ "updating " <> T.pack (show $ M.size battles') <> " battles."
  forM_ (M.keys battles') $ updateBattle dt

-- Update all NPCs
-- characters . traverse %= updateCharacter dt

-- | Update the battle state
updateBattle :: Double -> BattleId -> GameStateT ()
updateBattle dt bId = do
  wrld <- use world
  battle <- getsBattle bId
  -- Update battle state
  randG <- getStdGen
  (battleOver, battle', bttlMsg) <- runCombat randG ExceptionInCombat wrld battle $ flushBattleTick dt
  tell bttlMsg
  if battleOver
    then battleSettlement (battle' ^. battleState . battleChar . charHP <= 0) battle'
    else do
      -- Update battle state
      battles . at bId .= Just battle'

battleSettlement :: Bool -> Battle -> GameStateT ()
battleSettlement win battle = do
  let player = battle ^. battleState . battleChar
  let enemy = battle ^. battleEnemyState . battleChar
  battles . at (battle ^. battleOwner) .= Nothing
  -- TODO: update player's state
  return ()
