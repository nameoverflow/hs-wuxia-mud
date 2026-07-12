{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (serverApplication, gameTickLoop, battleTickLoop) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Lens
import Control.Monad
import Data.Aeson (decode)
import Data.Map
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Logging
import Game.Entity
import Game.Message
import Game.World (maps)
import GamePlay
import GameState
import Network.WebSockets
import Networking
import Relude (ToText (toText))
import Database
import Game.CharacterCreation
import System.Environment (lookupEnv)

type ServerMap = M.Map PlayerId Connection

data PersistencePolicy
  = SaveOnAnyResponse
  | SaveDirtyPlayers

saveDir :: FilePath
saveDir = "saves"

devResetPassword :: Text
devResetPassword = "__dev_reset"

isDevModeEnabled :: IO Bool
isDevModeEnabled = do
  mode <- lookupEnv "MUD_DEV_MODE"
  return $ mode `elem` [Just "1", Just "true", Just "TRUE"]

clearPlayerRuntimeState :: PlayerId -> GameState -> GameState
clearPlayerRuntimeState uid gs =
  releasePlayerBattleLock uid gs
    & players . at uid .~ Nothing
    & battles . at uid .~ Nothing
    & stories . at uid .~ Nothing
    & isolatedBattles %~ S.delete uid
    & dirtyPlayers %~ S.delete uid
    & world . maps . traversed . mapRooms . traversed . roomPlayer %~ S.delete uid

loadOrCreatePlayer :: Bool -> Maybe CharacterCreationChoice -> PlayerId -> GameState -> IO GameState
loadOrCreatePlayer resetPlayer creationChoice uid gs = do
  when resetPlayer $ do
    deletePlayerSave saveDir uid
    logInfo $ "Dev reset player save: " <> uid
  saveResult <-
    if resetPlayer
      then pure $ Right Nothing
      else loadPlayerSave saveDir uid
  let templateCreation =
        case saveResult of
          Right Nothing -> creationChoice
          _ -> Nothing
      gsm = createDefaultPlayerWithCreation uid "resources/scripts/default_player.yaml" templateCreation
      baseState =
        if resetPlayer
          then clearPlayerRuntimeState uid gs
          else gs
  runGameState baseState gsm >>= \case
    Left err -> do
      logError $ "Error loading player: " <> toText err
      return baseState
    Right (_, gs') -> do
      case saveResult of
        Left err -> do
          logError $ "Error loading player save: " <> err
          return gs'
        Right Nothing ->
          return gs'
        Right (Just save) ->
          return $ applyPlayerSaveToGameState save gs'

sendResp :: Connection -> ActionResp -> IO ()
sendResp conn resp = do
  msg <- formatResp resp
  sendTextData conn msg

sendCharacterCreationConfig :: Connection -> IO ()
sendCharacterCreationConfig conn = do
  loadCharacterCreationConfig characterCreationConfigPath >>= \case
    Left err ->
      sendResp conn $ ErrorMsg $ ErrorSummary "character_creation_config_load_failed" $ M.singleton "error" err
    Right config ->
      sendResp conn $ CharacterCreationConfigMsg config

broadcastResp :: ActionResp -> ServerMap -> IO ()
broadcastResp resp clients = do
  message <- formatResp resp
  logDebug $ "broadcastResp: " <> message
  forM_ clients $ \conn -> do
    sendTextData conn message

userLogin :: PlayerId -> Bool -> Maybe CharacterCreationChoice -> Connection -> MVar ServerMap -> MVar GameState -> IO ()
userLogin user resetPlayer creationChoice conn cm s = do
  modifyMVar_ cm $ \c -> do
    broadcastResp (SystemMsg $ SystemMessage "user_joined" $ M.singleton "user" user) c
    sendResp conn $
      SystemMsg $
        SystemMessage
          "welcome"
          (M.singleton "users" $ T.intercalate ", " (keys c))
    return $ M.insert user conn c
  modifyMVar_ s $ \ss -> do
    s' <- loadOrCreatePlayer resetPlayer creationChoice user ss
    logInfo $ "User logged in: " <> user <> if resetPlayer then " (dev reset)" else ""
    logDebug $ "players: " <> toText (show (keys . view players $ s'))
    return s'
  runAndResponse s cm (playerView user) $ \err -> do
    sendResp conn $ ErrorMsg $ gameExceptionToSummary err


serverApplication :: MVar ServerMap -> MVar GameState -> ServerApp
serverApplication conns state pending = do
  logInfo "Accepted WebSocket connection"
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    msg <- receiveData conn
    c <- readMVar conns
    case decode msg :: Maybe NetEvent of
      Just RequestCharacterCreationConfig ->
        sendCharacterCreationConfig conn
      Just Login {username = loginUser, password = pw, creation = creationChoice}
        | member loginUser c -> do
          sendResp conn $ ErrorMsg $ ErrorSummary "user_exists" $ M.singleton "user" loginUser
          fail "User already exists"
        | pw == devResetPassword -> do
          devMode <- isDevModeEnabled
          if devMode
            then flip finally (disconnectClient loginUser conns state) $ do
              userLogin loginUser True creationChoice conn conns state
              runGameLoop loginUser conns state
            else do
              sendResp conn $ ErrorMsg $ ErrorSummary "dev_mode_required" M.empty
              fail "Dev reset requires MUD_DEV_MODE=1"
        | otherwise -> flip finally (disconnectClient loginUser conns state) $ do
          userLogin loginUser False creationChoice conn conns state
          runGameLoop loginUser conns state
      _ -> do
        sendResp conn $ ErrorMsg $ ErrorSummary "not_logged_in" M.empty
        return ()

disconnectClient :: Text -> MVar ServerMap -> MVar GameState -> IO ()
disconnectClient user conns state = do
  -- Remove client and return new state
  s <- modifyMVar conns $ \s -> do
    let s' = M.delete user s
    return (s', s')
  modifyMVar_ state $ \gs ->
    do
      let gs' =
            releasePlayerBattleLock user gs
              & battles . at user .~ Nothing
              & players . ix user . playerStatus .~ PlayerNormal
      savePlayerState saveDir user gs'
      return gs'
  logInfo $ user <> " disconnected"
  broadcastResp (SystemMsg $ SystemMessage "user_disconnected" $ M.singleton "user" user) s

runAndResponse :: MVar GameState -> MVar ServerMap -> GameStateT a -> (GameException -> IO ()) -> IO ()
runAndResponse =
  runAndResponseWithPersistence SaveOnAnyResponse

runAndResponseWithPersistence :: PersistencePolicy -> MVar GameState -> MVar ServerMap -> GameStateT a -> (GameException -> IO ()) -> IO ()
runAndResponseWithPersistence policy state conns gameM onError = do
  modifyMVar_ state $ \s -> do
    runGameState s gameM >>= \case
      Left err -> do
        onError err
        return s
      Right (resp, s') -> do
        logDebug $ "runAndResponse: dispatching " <> toText (show (length resp)) <> " responses"
        withMVar conns $ dispatchResp resp
        saveGameState policy resp s'

saveGameState :: PersistencePolicy -> [PlayerResp] -> GameState -> IO GameState
saveGameState policy resp s = do
  let dirty = s ^. dirtyPlayers
      clearDirty = dirtyPlayers .~ S.empty
  case policy of
    SaveOnAnyResponse -> do
      unless (Prelude.null resp && S.null dirty) $
        saveAllPlayerSaves saveDir s
      return $ clearDirty s
    SaveDirtyPlayers -> do
      forM_ dirty $ \pid ->
        savePlayerState saveDir pid s
      return $ clearDirty s

dispatchResp :: Foldable m => m PlayerResp -> ServerMap -> IO ()
dispatchResp rsp conns = do
  forM_ rsp $ \(uid, resp) -> do
    logDebug $ "dispatchResp: sending to " <> uid <> ": " <> toText (show resp)
    case conns !? uid of
      Nothing -> logError $ "dispatchResp: player " <> uid <> " not found in connections"
      Just conn -> do
        msg <- formatResp resp
        logDebug $ "dispatchResp: formatted message: " <> msg
        sendTextData conn msg

runGameLoop :: Text -> MVar ServerMap -> MVar GameState -> IO ()
runGameLoop user conns state = do
  c_ <- readMVar conns
  forM_ (c_ !? user) receiveMsg
  where
    receiveMsg :: Connection -> IO ()
    receiveMsg conn = do
      logDebug $ user <> " waiting for message..."
      msg <- receiveData conn
      logDebug $ user <> " received: " <> toText (show msg)
      case decode msg :: Maybe NetEvent of
        Just Disconnect -> do
          logInfo $ user <> " disconnecting..."
          return ()
        Just (NetPlayerAction action) -> do
          logDebug $ user <> " action: " <> toText (show action)
          processAction conn action
          runGameLoop user conns state
        _ -> do
          logError $ user <> " invalid message, continuing..."
          runGameLoop user conns state

    processAction :: Connection -> PlayerAction -> IO ()
    processAction conn action = do
      logDebug $ user <> " processing action..."
      let gameM = processPlayerAction user action
      runAndResponse state conns gameM $ \err -> do
        logError $ user <> " action failed: " <> toText (show err)
        sendResp conn $ ErrorMsg $ gameExceptionToSummary err

gameTickLoop :: MVar ServerMap -> MVar GameState -> IO ()
gameTickLoop = runTickLoop "Game tick failed" 1000000 onGameTick

battleTickLoop :: MVar ServerMap -> MVar GameState -> IO ()
battleTickLoop = runTickLoop "Battle tick failed" 50000 onBattleTick

runTickLoop :: Text -> Int -> (Double -> GameStateT ()) -> MVar ServerMap -> MVar GameState -> IO ()
runTickLoop errorMessage interval tickAction cs gs = do
  currentTime <- getCurrentTime
  -- Sleep for the given interval (in milliseconds)
  threadDelay interval
  loop currentTime
  where
    loop :: UTCTime -> IO ()
    loop lastTime = do
      -- Get the current time
      currentTime <- getCurrentTime
      let diff = diffUTCTime currentTime lastTime
      -- Run the game tick
      let gsm = tickAction $ realToFrac diff
      runAndResponseWithPersistence SaveDirtyPlayers gs cs gsm $ \err -> do
        logError $ errorMessage <> ": " <> toText (show err)
      threadDelay interval
      -- Loop again
      loop currentTime
