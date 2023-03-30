{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (serverApplication) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Data.Aeson (decode)
import Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Builder (toLazyText)
import Data.Time
import GameState
import Network.WebSockets
import Networking

type ConnectionMap = M.Map Connection Text

addPlayer :: Connection -> GameState -> IO (GameState, Text)
addPlayer conn gameState = do
  playerId <- generateUniquePlayerId
  let newPlayer = createNewPlayer playerId
  let updatedPlayers = Map.insert playerId newPlayer (players gameState)
  let updatedGameState = gameState {players = updatedPlayers}
  return (updatedGameState, playerId)

registerConnection :: Connection -> Text -> ConnectionMap -> ConnectionMap
registerConnection conn playerId = Map.insert conn playerId

getTicks :: IO Double
getTicks = do
  now <- getCurrentTime
  let dayTime = utctDayTime now
      rationalDayTime = toRational dayTime
      integralVal = fromRational rationalDayTime
  return integralVal

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  TIO.putStrLn message
  forM_ clients $ \(_, conn) -> sendTextData conn message

type ServerState = HM.HashMap Text (PlayerState, Connection)

serverApplication :: MVar ServerState -> ServerApp
serverApplication state pending = do
  putStrLn "Accepted connection"
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    msg <- receiveData conn
    s <- readMVar state
    case decode msg :: Maybe NetEvent of
      Just Login {userName = un}
        | member un s -> do
          sendTextData conn ("User already exists" :: Text)
          fail "User already exists"
        | otherwise -> flip finally (disconnectClient un state) $ do
          modifyMVar_ state $ \s -> do
            player <- loadPlayerState un
            let s' = insert un (player, conn) s
            sendTextData conn $
              "Welcome! Users: "
                <> T.intercalate
                  ", "
                  (keys s)
            broadcast (un <> " joined") s'
            return s'
          runGameLoop un state
      _ -> do
        sendTextData conn ("Not Logged In" :: Text)
        return ()

disconnectClient :: Text -> MVar ServerState -> IO ()
disconnectClient user state = do
  -- Remove client and return new state
  s <- modifyMVar state $ \s -> do
    let s' = delete user s
    return (s', s')
  broadcast (user <> " disconnected") s

runGameLoop :: Text -> MVar ServerState -> IO ()
runGameLoop user state = do
  s_ <- readMVar state
  let (player, conn) = s_ ! user
  msg <- receiveData conn
  case decode msg :: Maybe NetEvent of
    Just Disconnect -> disconnectClient user state
    Just (Message msg) -> do
      modifyMVar_ state $ \s -> do
        case msg of
          Look -> _
          Status -> _
          Item -> _
          Skill -> _
          Control pc -> _
        return s
      runGameLoop user state
    _ -> return ()

route :: NetMessage -> PlayerState -> Connection -> MVar ServerState -> IO ServerState
route Look player conn server = do
  sendTextData conn ((T.pack . show $ playerMap player) <> (T.pack . show $ playerPosition player))
  readMVar server
route Status player conn server = do
  sendTextData conn $ T.pack . show $ player
  readMVar server
route Item player conn server = do
  semdTextData conn $ T.pack . show $ playerI
route Skill player conn server = readMVar server
route (Control pc) player conn server = readMVar server

-- route :: NetMessage -> MVar PlayerState -> IO ()
-- route msg = do
