{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Combat where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.RWS (RWST (..), MonadWriter (tell))
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict (MonadState, StateT (..))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Game.Entity
import Game.Message
import Game.World (World, effects, martialArts)
import Utils
import Relude (ToText (..), whenNothing)

type BattleId = Text

maxAp :: Int
maxAp = 100

data BattleState = BattleState
  { _battleActiveSkillCooldowns :: M.Map ActiveSkillId Double,
    _battleAp :: Int,
    _battleQi :: Int,
    _battleChar :: Character,
    _battleEffects :: M.Map EffectId ActiveEffect
  }
  deriving (Show, Eq, Generic)

data Battle = Battle
  { _battleOwner :: PlayerId,
    _battleState :: !BattleState,
    _battleEnemyState :: !BattleState
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
        { _battleActiveSkillCooldowns = M.empty,
          _battleAp = 0,
          _battleQi = _charQi char,
          _battleChar = char,
          _battleEffects = M.empty
        }

data CombatException = CombatException Text
  deriving (Show, Eq, Generic)

instance Exception CombatException
instance ToText CombatException where
  toText (CombatException msg) = msg

newtype Combat a = Combat
  { unCombat :: RWST World [PlayerResp] Battle (ExceptT CombatException (Rand StdGen)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Battle,
      MonadReader World,
      MonadError CombatException,
      MonadRandom,
      MonadWriter [PlayerResp]
    )

runCombat :: (MonadError e m) => StdGen -> (CombatException -> e) -> World -> Battle -> Combat a -> m (a, Battle, [PlayerResp])
runCombat rand fCatch world battle combat = do
  let (action, _) = runRand (runExceptT (runRWST (unCombat combat) world battle)) rand
  liftEither $ either (Left . fCatch) Right action

-- | Run a combat action, return if the battle is over
flushBattleTick :: Double -> Combat Bool
flushBattleTick dt = do
  char <- use $ battleState . battleChar
  enemy <- use $ battleEnemyState . battleChar

  -- Update active skill cooldowns
  battleState . battleActiveSkillCooldowns %= M.filter (> 0.0) . M.map (subtract dt)
  battleEnemyState . battleActiveSkillCooldowns %= M.filter (> 0.0) . M.map (subtract dt)

  -- Apply and expire active effects.
  applyActiveEffects dt battleState
  applyActiveEffects dt battleEnemyState
  expireActiveEffects dt battleState
  expireActiveEffects dt battleEnemyState

  playerHpAfterEffects <- use $ battleState . battleChar . charHP
  enemyHpAfterEffects <- use $ battleEnemyState . battleChar . charHP
  if playerHpAfterEffects <= 0 || enemyHpAfterEffects <= 0
    then return True
    else do
      -- Regenerate Qi
      battleState . battleQi += round (dt * _charQiRegen char)
      battleEnemyState . battleQi += round (dt * _charQiRegen enemy)

      -- Cap Qi at max
      battleState . battleQi %= min (_charMaxQi char)
      battleEnemyState . battleQi %= min (_charMaxQi enemy)

      -- Accumulate action points
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
          return $ playerHp <= 0

applyActiveEffects :: Double -> Lens' Battle BattleState -> Combat ()
applyActiveEffects dt state = do
  active <- use $ state . battleEffects
  effectDefs <- view effects
  uid <- use battleOwner
  targetName <- use $ state . battleChar . charName
  forM_ active $ \activeEffect -> do
    let amount = max 0 . round $ dt * fromIntegral (activeEffect ^. activeEffectValue)
    case M.lookup (activeEffect ^. activeEffectDef) effectDefs of
      Just effect
        | amount > 0 ->
            case effect ^. effectType of
              DoT -> do
                state . battleChar . charHP -= amount
                tell [(uid, CombatNormalMsg (effect ^. effectName) targetName (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "dot" amount) amount)]
              HoT -> do
                maxHp <- use $ state . battleChar . charMaxHP
                state . battleChar . charHP %= min maxHp . (+ amount)
                tell [(uid, ActiveSkillMsg (effect ^. effectName) targetName (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "hot" amount))]
              Buff -> return ()
              DeBuff -> return ()
      _ -> return ()

expireActiveEffects :: Double -> Lens' Battle BattleState -> Combat ()
expireActiveEffects dt state =
  state . battleEffects %= M.filter ((> 0.0) . _activeEffectRemaining) . M.map (\e -> e & activeEffectRemaining %~ subtract dt)

checkApAndAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
checkApAndAttack left right = do
  leftAp <- use $ left . battleAp
  when (leftAp >= maxAp) $ do
    left . battleAp .= 0
    (left . battleChar) `battleAttack` (right . battleChar)

battleAttack :: Lens' Battle Character -> Lens' Battle Character -> Combat ()
battleAttack left right = do
  attacker <- use left
  martialArtMap <- view martialArts
  let eqMoves = unlockedAttackMoves martialArtMap attacker

  -- randomly select a move
  move <- randomSelect eqMoves
  case move of
    Nothing -> do
      throwError $ CombatException $ pack $ "No unlocked move selected, prepared arts: " <> show (attacker ^. charPrepare)
    Just mv -> do
      -- apply damage
      let damage = mv ^. attackMoveDamage
      right . charHP -= damage

      -- write attack message
      uid <- use battleOwner
      leftName <- use $ left . charName
      rightName <- use $ right . charName
      let mvMsg = mv ^. attackMoveMsg
      tell [(uid, CombatNormalMsg leftName rightName (CombatScriptText mvMsg) damage)]

unlockedAttackMoves :: M.Map ArtId MartialArt -> Character -> [AttackMove]
unlockedAttackMoves martialArtMap char =
  [ move
    | artType' <- [Sword, Fist],
      Just artEntity <- [char ^. charPrepare . at artType'],
      Just martialArt <- [M.lookup (artEntity ^. artDef) martialArtMap],
      move <- martialArt ^. artAttackMoves,
      move ^. attackMoveUnlockLevel <= artEntity ^. artLevel
  ]

canUseActiveSkill :: ActiveSkill -> Lens' Battle BattleState -> Combat Bool
canUseActiveSkill activeSkill state = do
  apValue <- use $ state . battleAp
  qi <- use $ state . battleQi
  cds <- use $ state . battleActiveSkillCooldowns
  effects <- use $ state . battleEffects

  let hasEnoughAp = apValue >= activeSkill ^. activeSkillApReq
      hasEnoughQi = qi >= activeSkill ^. activeSkillCost
      isOffCooldown = not $ M.member (activeSkill ^. activeSkillId) cds
      hasRequiredEffects = all (`M.member` effects) (activeSkill ^. activeSkillReqStatus)

  return $ hasEnoughAp && hasEnoughQi && isOffCooldown && hasRequiredEffects

useActiveSkill :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
useActiveSkill activeSkill caster target = do
  let effectiveTarget =
        case activeSkill ^. activeSkillTarget of
          Self -> caster
          _ -> target

  -- Consume AP
  caster . battleAp %= max 0 . subtract (activeSkill ^. activeSkillApReq)

  -- Consume Qi
  caster . battleQi -= activeSkill ^. activeSkillCost

  -- Set cooldown
  caster . battleActiveSkillCooldowns . at (activeSkill ^. activeSkillId) .= Just (activeSkill ^. activeSkillCooldown)

  -- Apply effects based on target type
  case activeSkill ^. activeSkillTarget of
    Single -> applyActiveSkillEffects activeSkill caster target
    Self   -> applyActiveSkillEffects activeSkill caster caster
    All    -> applyActiveSkillEffects activeSkill caster target  -- For now, same as Single

  -- Generate active skill message
  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <- use $ effectiveTarget . battleChar . charName
  tell [(uid, ActiveSkillMsg casterName targetName (CombatScriptText $ activeSkill ^. activeSkillMsg))]

applyActiveSkillEffects :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
applyActiveSkillEffects activeSkill caster target = do
  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <- use $ target . battleChar . charName

  -- Apply damage
  case activeSkill ^. activeSkillDamage of
    Just dmg -> do
      target . battleChar . charHP -= dmg
      tell [(uid, CombatNormalMsg casterName targetName (CombatScriptText $ activeSkill ^. activeSkillMsg) dmg)]
    Nothing -> return ()

  -- Apply healing
  case activeSkill ^. activeSkillHeal of
    Just heal -> do
      maxHp <- use $ target . battleChar . charMaxHP
      target . battleChar . charHP %= min maxHp . (+ heal)
      -- TODO: Add proper healing message
    Nothing -> return ()

  -- Apply effects to target
  forM_ (activeSkill ^. activeSkillEffTarget) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    target . battleEffects . at effId .= Just activeEff

  -- Apply effects to caster (self-buffs)
  forM_ (activeSkill ^. activeSkillEffSelf) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    caster . battleEffects . at effId .= Just activeEff
