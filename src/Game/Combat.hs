{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Game.Combat where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.RWS (RWST (..))
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict (MonadState, StateT (..))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.Entity
import Game.World (World, skills)
import Utils

type BattleId = Text

maxAp :: Int
maxAp = 100

data BattleState = BattleState
  { _battleSkillCd :: M.Map SkillId Double,
    _battleAp :: Int,
    _battleChar :: Character
    -- _battleEffects :: M.Map EffectId Effect
  }
  deriving (Show, Eq, Generic)

data Battle = Battle
  { _battleOwner :: PlayerId,
    _battleState :: BattleState,
    _battleEnemyState :: BattleState
  }
  deriving (Show, Eq, Generic)

makeLenses ''Battle

makeLenses ''BattleState

newBattle :: Player -> Character -> Battle
newBattle player npc =
  Battle
    { _battleOwner = _playerId player,
      _battleState = newBattleState $ _playerCharacter player,
      _battleEnemyState = newBattleState npc
    }
  where
    newBattleState char =
      BattleState
        { _battleSkillCd = M.empty,
          _battleAp = 0,
          _battleChar = char
        }

data CombatException = CombatException Text
  deriving (Show, Eq, Generic)

-- newtype Combat a = Combat
--   { unCombat :: ReaderT World (StateT Battle (ExceptT CombatException Identity)) a
--   }
--   deriving (Functor, Applicative, Monad, MonadState Battle, MonadReader World, MonadError CombatException)
newtype Combat a = Combat
  { unCombat :: RWST World () Battle (ExceptT CombatException (Rand StdGen)) a
  } deriving (Functor, Applicative, Monad, MonadState Battle, MonadReader World, MonadError CombatException, MonadRandom)

runCombat :: (MonadError e m) => StdGen -> (CombatException -> e) -> World -> Battle -> Combat a -> m (a, Battle, ())
runCombat rand fCatch world battle combat = do
  let (action, _) = runRand (runExceptT (runRWST (unCombat combat) world battle)) rand
  liftEither (fmapL fCatch action)
  where
    fmapL :: (a -> b) -> Either a r -> Either b r
    fmapL f = either (Left . f) Right


-- | Run a combat action, return if the battle is over
flushBattleTick :: Double -> Combat Bool
flushBattleTick dt = do
  char <- use $ battleState . battleChar
  enemy <- use $ battleEnemyState . battleChar
  battleState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)
  battleEnemyState . battleSkillCd %= M.filter (> 0.0) . M.map (subtract dt)

  -- action points moving on
  battleState . battleAp += round (dt * fromIntegral (_charAgility char))
  battleEnemyState . battleAp += round (dt * fromIntegral (_charAgility enemy))

  -- Check whether to attack the enemy
  checkApAndAttack battleState battleEnemyState
  enemyHp <- use $ battleEnemyState . battleChar . charHP
  if enemyHp <= 0
    then return True
    else do
      checkApAndAttack battleEnemyState battleState
      playerHp <- use $ battleState . battleChar . charHP
      return $ playerHp > 0

checkApAndAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
checkApAndAttack left right = do
  leftAp <- use $ left . battleAp
  when (leftAp >= maxAp) $ do
    left . battleAp .= 0
    (left . battleChar) `battleAttack` (right . battleChar)

-- | Random select a move from player's skill to attack
battleAttack :: Lens' Battle Character -> Lens' Battle Character -> Combat ()
battleAttack left right = do
  eqTechId <- use $ left . charEqTech . techDef
  eqMoves <- view $ skills . at eqTechId . _Just . techMoves

  -- randomly select a move
  move <- randomSelect eqMoves

  -- apply damage
  let damage = move ^. moveDamage
  right . charHP -= damage
