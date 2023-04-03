{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens
import qualified Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), withExceptT)
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
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
  { unGameStateT :: WriterT [ActionResp] (StateT GameState (ExceptT GameException IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadState GameState, MonadError GameException, MonadWriter [ActionResp], MonadIO)

getsPlayer :: PlayerId -> GameStateT Player
getsPlayer curId = getsL players curId (PlayerNotFound curId)

getsBattle :: BattleId -> GameStateT Battle
getsBattle bId = getsL battles bId $ BattleNotFound bId

liftWorld :: WorldStateT a -> GameStateT a
liftWorld m = GameStateT . lift $ mapStateT (withExceptT ExceptionInWorld) $ zoom world m
