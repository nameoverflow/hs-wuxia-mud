{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameState where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), withExceptT, runExceptT)
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer
import qualified Data.Map.Strict as M
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
import qualified Data.Set as S
import Relude (ToText)
import Relude.String.Conversion (ToText(..))

data GameState = GameState
  { -- world properties
    _world :: !World,
    -- game status
    _players :: !(M.Map PlayerId Player),
    _battles :: !(M.Map PlayerId Battle),
    _respawn :: !(M.Map CharId Double)
  }
  deriving (Show, Eq, Generic)

makeLenses ''GameState

data GameException
  = PlayerNotFound PlayerId
  | BattleNotFound BattleId
  | UnableToInteract Character CharAction
  | UnableToMove Direction Room
  | ExceptionInWorld WorldException
  | ExceptionInCombat CombatException
  | OtherException Text
  deriving (Show, Eq, Generic)

instance ToText GameException where
  toText = \case
    PlayerNotFound pid -> "Player not found: " <> toText pid
    BattleNotFound bid -> "Battle not found: " <> toText bid
    UnableToInteract char act -> "Unable to " <> toText (Prelude.show act) <> ": " <> toText (_charName char)
    UnableToMove dir room -> "Unable to move " <> toText (Prelude.show dir) <> " in " <> toText (_roomName room)
    ExceptionInWorld err -> "Exception in world: " <> toText err
    ExceptionInCombat err -> "Exception in combat: " <> toText err
    OtherException err -> "Other exception: " <> err

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
newGameState w = GameState w M.empty M.empty M.empty

loadGameState :: FilePath -> IO (Either Text GameState)
loadGameState basePath = do
  worldResult <- loadAllAssets basePath
  return $ newGameState <$> worldResult

createDefaultPlayer :: PlayerId -> FilePath -> GameStateT ()
createDefaultPlayer pid path = do
  playerResult <- liftIO $ loadConfigFrom path
  case playerResult of
    Left err -> throwError $ OtherException err
    Right (player :: Player) -> do
      let player' = player & playerId .~ pid & playerCharacter . charId .~ "player$" <> pid
      players . at pid .= Just player'

      let (mid, pos) = player ^. playerPosition
      world . maps . ix mid . mapRooms . ix pos . roomPlayer %= S.insert pid
