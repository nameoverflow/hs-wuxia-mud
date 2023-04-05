{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError, withExceptT, runExceptT)
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer
import qualified Data.Map as M
import Data.Text
import GHC.Generics (Generic)
import Game.Combat
import Game.Entity
import Game.Quest
import Game.World
import Relude.Monad (leftToMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Utils
import Control.Monad.RWS (MonadWriter)
import Game.Message

data GameState = GameState
  { -- world properties
    _world :: World,
    -- game status
    _players :: M.Map PlayerId Player,
    _battles :: M.Map PlayerId Battle
  }
  deriving (Show, Eq, Generic)

makeLenses ''GameState

data GameException
  = PlayerNotFound PlayerId
  | BattleNotFound BattleId
  | UnableToAttack Character
  | UnableToMove Direction Room
  | ExceptionInWorld WorldException
  | ExceptionInCombat CombatException
  | OtherException Text
  deriving (Show, Eq, Generic)

newtype GameStateT a = GameStateT
  { unGameStateT :: WriterT [PlayerResp] (StateT GameState (Control.Monad.Except.ExceptT GameException IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadState GameState, Control.Monad.Except.MonadError GameException, MonadWriter [PlayerResp], MonadIO)

getsPlayer :: PlayerId -> GameStateT Player
getsPlayer curId = getsL players curId (PlayerNotFound curId)

getsBattle :: BattleId -> GameStateT Battle
getsBattle bId = getsL battles bId $ BattleNotFound bId

liftWorld :: WorldStateT a -> GameStateT a
liftWorld m = GameStateT . lift $ mapStateT (Control.Monad.Except.withExceptT ExceptionInWorld) $ zoom world m

runGameState :: (MonadIO m) => GameState -> GameStateT a -> m (Either GameException ([PlayerResp], GameState))
runGameState gs m = liftIO $ Control.Monad.Except.runExceptT $ runStateT (execWriterT $ unGameStateT m) gs

newGameState :: World -> GameState
newGameState w = GameState w M.empty M.empty

loadGameState :: FilePath -> IO (Either Text GameState)
loadGameState basePath = do
  worldResult <- loadAllAssets basePath
  return $ newGameState <$> worldResult
