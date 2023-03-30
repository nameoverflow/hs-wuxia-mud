{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module GamePlay
where

import Control.Lens
import qualified Control.Monad
import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (liftIO),
    MonadTrans (lift),
    unless,
    when,
  )
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Game.Entity
import Game.Combat
import GameState
import Control.Monad.State.Strict (runState)
import Utils
import qualified Data.Map as M
import Control.Monad (when, forM_)
import Game.World
import qualified Data.Set as S
import Control.Monad.Random (randomIO)

-- import Script

processPlayerNormalAction :: T.Text -> PlayerAction -> GameStateT ()
processPlayerNormalAction curPlayerId action = case action of
  Go direction -> playerMove curPlayerId direction
  Attack (target :: CharId) -> playerAttack curPlayerId target
  _ -> return ()


processPlayerBattleAction :: T.Text -> PlayerAction -> GameStateT ()
processPlayerBattleAction curPlayerId action = case action of
  ApplySkill skillId -> do
    player <- getsPlayer curPlayerId
    battle <- getsBattle curPlayerId
    let (battle', skill) = runState (useSkill skillId) battle
    reuturn ()

-- Use item -> do
--   -- Update player's inventory and apply item effects
--   players . ix playerId %= updatePlayerAfterUsingItem item
-- Say message -> do
--   -- Broadcast the message to other players in the same location
--   broadcastMessage message
-- Other command -> do
--   -- Handle custom commands, e.g., emotes or special actions
--   handleCustomCommand command

playerMove :: T.Text -> Direction -> GameStateT ()
playerMove curPlayerId direction = do
  (curMapId, curPos) <- (^. playerPosition) <$> getsPlayer curPlayerId
  currentRoom <- liftWorld $ getsMapRoom curMapId curPos
  let newPosition = currentRoom ^. roomExits . at direction
  case newPosition of
    Nothing -> throwError $ UnableToMove direction currentRoom
    Just posToGo -> do
      players . at curPlayerId . _Just . playerPosition .= (curMapId, posToGo)
      -- newRoom <- liftWorld $ getsMapRoom curMapId posToGo
      -- broadcastMessage $ T.concat [playerId, " has entered ", newRoom ^. roomName]
      world . maps . ix curMapId . mapRooms . ix posToGo . roomPlayer %= S.insert curPlayerId
      return ()

-- Create a new battle between the player and the target NPC
playerAttack :: PlayerId -> CharId -> GameStateT ()
playerAttack curPlayerId target = do
  player <- getsPlayer curPlayerId
  npc <- liftWorld $ getsCharacter target
  unless (_charAttackable npc) $ throwError $ UnableToAttack npc
  battles . at curPlayerId .= Just (newBattle player npc)

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
  -- Update all NPCs
  -- characters . traverse %= updateCharacter dt

-- | Update the battle state
updateBattle :: Double -> BattleId -> GameStateT ()
updateBattle dt bId = do
  world <- use world
  battle <- getsBattle bId
  -- Update battle state
  randG <- randomIO
  (battleOver, battle', ()) <- runCombat randG ExceptionInCombat world battle $ flushBattleTick dt
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
  return ()
  -- TODO: update player's state
