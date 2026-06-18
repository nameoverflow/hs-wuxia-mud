{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent
import Control.Exception (throwIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GameState (loadGameState)
import Logging
import Network.WebSockets
import Server
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)



main :: IO ()
main = do
  -- Set up buffered output
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  logInfo "Initializing game state..."
  -- Load game data from YAML files and create initial game state
  -- luaState <- newstate
  gameState <- loadGameState "resources/scripts" >>= \case
    Left err -> do
      let errMessage = "Failed to load game state: " <> show err
      logError $ T.pack errMessage
      throwIO $ userError errMessage
    Right gs -> return gs
  gameStateMVar <- newMVar gameState

  -- Initialize websocket server map
  conns <- newMVar M.empty

  logInfo "Starting game tick..."
  -- Run the game tick in a separate thread
  _ <- forkIO $ gameTickLoop conns gameStateMVar

  logInfo "Starting battle tick..."
  _ <- forkIO $ battleTickLoop conns gameStateMVar

  logInfo "Starting WebSocket server on 127.0.0.1:9160..."
  -- Initialize and run the WebSocket server
  runServer "127.0.0.1" 9160 $ serverApplication conns gameStateMVar

-- main :: IO ()
-- main = return ()
