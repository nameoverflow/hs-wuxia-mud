{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import GamePlay (onGameTick)
import GameState (GameState, runGameState, loadGameState)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Network.WebSockets
import Control.Concurrent
import Control.Monad
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (throwIO)
import qualified Data.Map.Strict as M
import Server



main :: IO ()
main = do
  -- Set up buffered output
  hSetBuffering stdout LineBuffering

  putStrLn "Initializing game state..."
  -- Load game data from YAML files and create initial game state
  -- luaState <- newstate
  gameState <- loadGameState "content" >>= \case
    Left err -> error $ "Failed to load game state: " <> show err
    Right gs -> return gs
  gameStateMVar <- newMVar gameState

  -- Initialize websocket server map
  conns <- newMVar M.empty

  putStrLn "Starting game tick..."
  -- Run the game tick in a separate thread
  _ <- forkIO $ gameTickLoop conns gameStateMVar

  putStrLn "Starting WebSocket server..."
  -- Initialize and run the WebSocket server
  runServer "127.0.0.1" 9160 $ serverApplication conns gameStateMVar

-- main :: IO ()
-- main = return ()