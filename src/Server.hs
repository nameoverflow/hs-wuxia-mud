{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (serverApplication, gameTickLoop) where

import Control.Concurrent
import Control.Monad
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import GameState
import Network.WebSockets
import Networking
import Data.Map
import qualified Data.Map.Strict as M
import Game.Entity
import GamePlay
import Control.Lens
import Relude (ToText(toText))
import Game.Message

type ConnectionMap = M.Map PlayerId Connection

addPlayer :: PlayerId -> Player -> GameState -> GameState
addPlayer uid p = players . at uid ?~ p

loadOrCreatePlayer :: PlayerId -> IO Player
loadOrCreatePlayer uid = return $ newPlayer uid "Player"

getTicks :: IO Double
getTicks = do
  now <- getCurrentTime
  let dayTime = utctDayTime now
      rationalDayTime = toRational dayTime
      integralVal = fromRational rationalDayTime
  return integralVal

broadcast :: Text -> ConnectionMap -> IO ()
broadcast message clients = do
  TIO.putStrLn message
  forM_ clients $ \conn -> do
    sendTextData conn message

userLogin :: PlayerId -> Connection -> MVar ConnectionMap -> MVar GameState -> IO ()
userLogin user conn cm s = do
  modifyMVar_ cm $ \c -> do
    broadcast (user <> " joined") c
    sendTextData conn $
      "Welcome! Users: "
        <> T.intercalate
          ", "
          (keys c)
    return $ M.insert user conn c
  modifyMVar_ s $ \st -> do
    return $ addPlayer user (newPlayer user "Player") st

serverApplication :: MVar ConnectionMap -> MVar GameState -> ServerApp
serverApplication conns state pending = do
  putStrLn "Accepted connection"
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    msg <- receiveData conn
    c <- readMVar conns
    case decode msg :: Maybe NetEvent of
      Just Login {userName = un}
        | member un c -> do
          sendTextData conn ("User already exists" :: Text)
          fail "User already exists"
        | otherwise -> do
          userLogin un conn conns state
          runGameLoop un conns state
      _ -> do
        sendTextData conn ("Not Logged In" :: Text)
        return ()

disconnectClient :: Text -> MVar ConnectionMap -> IO ()
disconnectClient user conns = do
  -- Remove client and return new state
  s <- modifyMVar conns $ \s -> do
    let s' = M.delete user s
    return (s', s')
  broadcast (user <> " disconnected") s

dispatchResp :: Foldable m => m PlayerResp -> ConnectionMap -> IO ()
dispatchResp rsp conns = do
  forM_ rsp $ \(uid, resp) -> do
    case conns !? uid of
      Nothing -> return ()
      Just conn -> do
        formatResp resp >>= sendTextData conn


runGameLoop :: Text -> MVar ConnectionMap -> MVar GameState -> IO ()
runGameLoop user conns state = do
  c_ <- readMVar conns
  forM_ (c_ !? user) receiveMsg
  where
    receiveMsg :: Connection -> IO ()
    receiveMsg conn = do
      msg <- receiveData conn
      case decode msg :: Maybe NetEvent of
        Just Disconnect -> disconnectClient user conns
        Just (NetPlayerAction action) -> processAction conn action
        _ -> runGameLoop user conns state

    processAction :: Connection -> PlayerAction -> IO ()
    processAction conn action = do
      modifyMVar_ state $ \s -> do
        let gameM = processPlayerNormalAction user action
        ret <- runGameState s gameM
        case ret of
          Left err -> do
            sendTextData conn $ "Error: " <> toText (show err)
            return s
          Right (resp, s') -> do
            withMVar conns $ dispatchResp resp
            return s'
      runGameLoop user conns state



gameTickLoop :: MVar ConnectionMap -> MVar GameState -> IO ()
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
      modifyMVar_ gs $ \s -> do
        res <- runGameState s gsm
        case res of
          Left err -> do
            putStrLn $ "Error: " <> show err
            return s
          Right (rsp, s') -> do
            withMVar cs $ dispatchResp rsp
            return s'
      threadDelay interval
      -- Loop again
      loop currentTime

