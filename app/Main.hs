module Main where

import qualified Connection
import Control.Concurrent.Async (async, wait)
import Control.Monad.IO.Class (liftIO)
import HsLua
import qualified Server
import System.IO (BufferMode (..), hSetBuffering, stdout)

gameTick :: StateT GameState IO ()
gameTick = do
  -- Perform periodic updates, such as NPC movement, weather changes, or other timed events
  modify' $ \gameState ->
    gameState
      { npcs = updateNPCs (npcs gameState),
        maps = updateMaps (maps gameState),
        quests = updateQuests (quests gameState)
      }

runGameTick :: Int -> StateT GameState IO ()
runGameTick interval = forever $ do
  -- Perform the game tick
  gameTick

  -- Sleep for the given interval (in milliseconds)
  liftIO $ threadDelay (interval * 1000)

main :: IO ()
main = do
  -- Set up buffered output
  hSetBuffering stdout LineBuffering

  putStrLn "Initializing game state..."
  -- Load game data from YAML files and create initial game state
  luaState <- newstate

  gameState <- loadInitialState

  putStrLn "Starting WebSocket server..."
  -- Initialize and run the WebSocket server
  serverAsync <- async $ Server.runServer gameState

  putStrLn "Starting game tick..."
  -- Set the game tick interval (in milliseconds)
  let tickInterval = 1000
  -- Run the game tick in a separate thread
  tickAsync <- async $ runGameTick tickInterval

  -- Wait for both threads to finish (which shouldn't happen under normal circumstances)
  waitEither serverAsync tickAsync
