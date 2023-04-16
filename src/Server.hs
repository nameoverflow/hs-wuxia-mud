{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (serverApplication, gameTickLoop) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Aeson (decode)
import Data.Map
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Game.Entity
import Game.Message
import GamePlay
import GameState
import Network.WebSockets
import Networking
import Relude (ToText (toText))
import Control.Exception (finally)

type ServerMap = M.Map PlayerId Connection

loadOrCreatePlayer :: PlayerId -> GameState -> IO GameState
loadOrCreatePlayer uid gs = do
  let gsm = createDefaultPlayer uid "resources/scripts/default_player.yaml"
  runGameState gs gsm >>= \case
    Left err -> do
      TIO.putStrLn $ "Error loading player: " <> toText err
      return gs
    Right (_, gs') -> do
      return gs'

broadcast :: Text -> ServerMap -> IO ()
broadcast message clients = do
  TIO.putStrLn message
  forM_ clients $ \conn -> do
    sendTextData conn message

userLogin :: PlayerId -> Connection -> MVar ServerMap -> MVar GameState -> IO ()
userLogin user conn cm s = do
  modifyMVar_ cm $ \c -> do
    broadcast (user <> " joined") c
    sendTextData conn $
      "Welcome! Users: "
        <> T.intercalate
          ", "
          (keys c)
    return $ M.insert user conn c
  modifyMVar_ s $ \ss -> do
    s' <- loadOrCreatePlayer user ss
    TIO.putStrLn $ "players: " <> toText (show (keys . view players $ s'))
    return s'
  runAndResponse s cm (playerView user) $ \err -> do
    sendTextData conn $ "Error: " <> toText (show err)


serverApplication :: MVar ServerMap -> MVar GameState -> ServerApp
serverApplication conns state pending = do
  putStrLn "Accepted connection"
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    msg <- receiveData conn
    c <- readMVar conns
    case decode msg :: Maybe NetEvent of
      Just Login {username = un}
        | member un c -> do
          sendTextData conn ("User already exists" :: Text)
          fail "User already exists"
        | otherwise -> flip finally (disconnectClient un conns) $ do
          userLogin un conn conns state
          runGameLoop un conns state
      _ -> do
        sendTextData conn ("Not Logged In" :: Text)
        return ()

disconnectClient :: Text -> MVar ServerMap -> IO ()
disconnectClient user conns = do
  -- Remove client and return new state
  s <- modifyMVar conns $ \s -> do
    let s' = M.delete user s
    return (s', s')
  TIO.putStrLn $ user <> " disconnected"
  broadcast (user <> " disconnected") s

runAndResponse :: MVar GameState -> MVar ServerMap -> GameStateT a -> (GameException -> IO ()) -> IO ()
runAndResponse state conns gameM onError = do
  modifyMVar_ state $ \s -> do
    runGameState s gameM >>= \case
      Left err -> do
        onError err
        return s
      Right (resp, s') -> do
        withMVar conns $ dispatchResp resp
        return s'

dispatchResp :: Foldable m => m PlayerResp -> ServerMap -> IO ()
dispatchResp rsp conns = do
  forM_ rsp $ \(uid, resp) -> do
    case conns !? uid of
      Nothing -> return ()
      Just conn -> do
        formatResp resp >>= sendTextData conn

runGameLoop :: Text -> MVar ServerMap -> MVar GameState -> IO ()
runGameLoop user conns state = do
  c_ <- readMVar conns
  forM_ (c_ !? user) receiveMsg
  where
    receiveMsg :: Connection -> IO ()
    receiveMsg conn = do
      msg <- receiveData conn
      case decode msg :: Maybe NetEvent of
        Just Disconnect -> disconnectClient user conns
        Just (NetPlayerAction action) -> do
          processAction conn action
          runGameLoop user conns state
        _ -> runGameLoop user conns state

    processAction :: Connection -> PlayerAction -> IO ()
    processAction conn action = do
      let gameM = processPlayerNormalAction user action
      runAndResponse state conns gameM $ \err -> do
        sendTextData conn $ "Error: " <> toText (show err)

gameTickLoop :: MVar ServerMap -> MVar GameState -> IO ()
gameTickLoop cs gs = do
  currentTime <- getCurrentTime
  -- Sleep for the given interval (in milliseconds)
  threadDelay interval
  loop currentTime
  where
    -- 0.1 seconds
    interval = 100
    loop :: UTCTime -> IO ()
    loop lastTime = do
      -- Get the current time
      currentTime <- getCurrentTime
      let diff = diffUTCTime currentTime lastTime
      -- Run the game tick
      let gsm = onGameTick $ realToFrac diff
      runAndResponse gs cs gsm $ \err -> do
        putStrLn $ "Error: " <> show err
      threadDelay interval
      -- Loop again
      loop currentTime
