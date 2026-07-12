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
import Game.World (World, combatActionTimings, effects, martialArts)
import Utils
import Relude (ToText (..), whenNothing)

type BattleId = Text

maxAp :: Int
maxAp = 100

targetCombatantActionSeconds :: Double
targetCombatantActionSeconds = 2.0

baselineAgility :: Double
baselineAgility = 19.0

apGainRate :: Double
apGainRate = fromIntegral maxAp / (targetCombatantActionSeconds * baselineAgility)

data BattleState = BattleState
  { _battleActiveSkillCooldowns :: M.Map ActiveSkillId Double,
    _battleAp :: Int,
    _battleApProgress :: Double,
    _battleQi :: Int,
    _battleCombatExp :: Int,
    _battleChar :: Character,
    _battleEffects :: M.Map EffectId ActiveEffect
  }
  deriving (Show, Eq, Generic)

data Battle = Battle
  { _battleOwner :: PlayerId,
    _battleState :: !BattleState,
    _battleEnemyState :: !BattleState,
    _battleActionLockRemaining :: !Double,
    _battlePendingActiveSkill :: Maybe PendingActiveSkill
  }
  deriving (Show, Eq, Generic)

data PendingActiveSkill = PendingActiveSkill
  { _pendingActiveSkillMartialArt :: MartialArt,
    _pendingActiveSkillSkill :: ActiveSkill
  }
  deriving (Show, Eq, Generic)

data PreparedAttack = PreparedAttack
  { preparedAttackArt :: ArtEntity,
    preparedAttackMartialArt :: MartialArt,
    preparedAttackMove :: AttackMove
  }
  deriving (Show, Eq, Generic)

makeLenses ''Battle

makeLenses ''PendingActiveSkill

makeLenses ''BattleState

newBattle :: Player -> Character -> Battle
newBattle player npc =
  Battle
    { _battleOwner = _playerId player,
      _battleState = newBattleState (player ^. playerCharacter) (player ^. playerCombatExp),
      _battleEnemyState = newBattleState npc 0,
      _battleActionLockRemaining = 0,
      _battlePendingActiveSkill = Nothing
    }
  where
    newBattleState char combatExp =
      BattleState
        { _battleActiveSkillCooldowns = M.empty,
          _battleAp = 0,
          _battleApProgress = 0,
          _battleQi = _charQi char,
          _battleCombatExp = combatExp,
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
  battleOver <- flushBattleMaintenanceTick dt
  if battleOver
    then return True
    else flushBattleActionTick dt

-- | Run slow resource/effect maintenance without advancing combat AP.
flushBattleMaintenanceTick :: Double -> Combat Bool
flushBattleMaintenanceTick dt = do
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
      playerStats <- battleDerivedStats battleState
      enemyStats <- battleDerivedStats battleEnemyState
      battleState . battleQi %= min (playerStats ^. dsMaxQi)
      battleEnemyState . battleQi %= min (enemyStats ^. dsMaxQi)
      return False

-- | Run fast AP/action advancement. At most one automatic action is emitted per tick.
flushBattleActionTick :: Double -> Combat Bool
flushBattleActionTick dt = do
  lockBefore <- use battleActionLockRemaining
  actionDt <- consumeBattleActionLock dt
  playerHpBefore <- use $ battleState . battleChar . charHP
  enemyHpBefore <- use $ battleEnemyState . battleChar . charHP
  if playerHpBefore <= 0 || enemyHpBefore <= 0
    then return True
    else if lockBefore > 0 && dt < lockBefore
      then return False
      else do
        pendingSkill <- use battlePendingActiveSkill
        case pendingSkill of
          Just pending -> do
            battlePendingActiveSkill .= Nothing
            useActiveSkill (pending ^. pendingActiveSkillMartialArt) (pending ^. pendingActiveSkillSkill) battleState battleEnemyState
            enemyHp <- use $ battleEnemyState . battleChar . charHP
            playerHp <- use $ battleState . battleChar . charHP
            return $ enemyHp <= 0 || playerHp <= 0
          Nothing -> do
            playerStats <- battleDerivedStats battleState
            enemyStats <- battleDerivedStats battleEnemyState

            -- Accumulate action points with fractional progress so high-frequency ticks
            -- preserve the same combat pace as the old second-based tick.
            accumulateAp actionDt playerStats battleState
            accumulateAp actionDt enemyStats battleEnemyState

            -- Check whether to attack the enemy
            playerActed <- checkApAndAttack battleState battleEnemyState
            enemyHp <- use $ battleEnemyState . battleChar . charHP
            if enemyHp <= 0
              then return True
              else do
                if playerActed
                  then return ()
                  else do
                    _ <- checkApAndAttack battleEnemyState battleState
                    return ()
                playerHp <- use $ battleState . battleChar . charHP
                return $ playerHp <= 0

consumeBattleActionLock :: Double -> Combat Double
consumeBattleActionLock dt = do
  lockRemaining <- use battleActionLockRemaining
  if lockRemaining <= 0
    then return dt
    else do
      let nextLock = lockRemaining - dt
      battleActionLockRemaining .= max 0 nextLock
      return $ max 0 (negate nextLock)

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
                visual <- effectTickVisual "dot"
                state . battleChar . charHP -= amount
                tell
                  [
                    ( uid,
                      combatEventResp
                        CombatEventEffectTick
                        (effect ^. effectName)
                        targetName
                        (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "dot" amount)
                        (Just amount)
                        Nothing
                        CombatHit
                        visual
                    )
                  ]
              HoT -> do
                visual <- effectTickVisual "hot"
                maxHp <- (^. dsMaxHp) <$> battleDerivedStats state
                state . battleChar . charHP %= min maxHp . (+ amount)
                tell
                  [
                    ( uid,
                      combatEventResp
                        CombatEventEffectTick
                        (effect ^. effectName)
                        targetName
                        (CombatEffectTick (activeEffect ^. activeEffectDef) (effect ^. effectName) "hot" amount)
                        Nothing
                        (Just amount)
                        CombatEffect
                        visual
                    )
                  ]
              Buff -> return ()
              DeBuff -> return ()
      _ -> return ()

expireActiveEffects :: Double -> Lens' Battle BattleState -> Combat ()
expireActiveEffects dt state =
  state . battleEffects %= M.filter ((> 0.0) . _activeEffectRemaining) . M.map (\e -> e & activeEffectRemaining %~ subtract dt)

battleDerivedStats :: Lens' Battle BattleState -> Combat DerivedStats
battleDerivedStats state = do
  char <- use $ state . battleChar
  combatExp <- use $ state . battleCombatExp
  activeEffects <- use $ state . battleEffects
  effectDefs <- view effects
  pure $ deriveCharacterStats char combatExp (characterDerivedStatSources effectDefs activeEffects char)

checkApAndAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat Bool
checkApAndAttack left right = do
  leftAp <- currentApProgress left
  if leftAp >= fromIntegral maxAp
    then do
      left . battleAp .= 0
      left . battleApProgress .= 0
      left `battleAttack` right
      return True
    else return False

currentApProgress :: Lens' Battle BattleState -> Combat Double
currentApProgress state = do
  visibleAp <- use $ state . battleAp
  progress <- use $ state . battleApProgress
  return $ max progress (fromIntegral visibleAp)

accumulateAp :: Double -> DerivedStats -> Lens' Battle BattleState -> Combat ()
accumulateAp dt stats state = do
  current <- currentApProgress state
  let nextAp = min (fromIntegral maxAp) (current + apGain dt stats)
  state . battleApProgress .= nextAp
  state . battleAp .= floor nextAp

apGain :: Double -> DerivedStats -> Double
apGain dt stats =
  dt * fromIntegral (stats ^. dsAgility) * apGainRate

combatEventResp :: CombatEventKind -> Text -> Text -> CombatMessage -> Maybe Int -> Maybe Int -> CombatResult -> CombatVisualHint -> ActionResp
combatEventResp kind actorName targetName message damage heal result visual =
  CombatEventMsg $
    CombatEvent
      { combatEventKind = kind,
        combatEventActorName = actorName,
        combatEventTargetName = targetName,
        combatEventMessage = message,
        combatEventDamage = damage,
        combatEventHeal = heal,
        combatEventResult = result,
        combatEventVisual = visual
      }

lockBattleForVisual :: CombatVisualHint -> Combat ()
lockBattleForVisual visual = do
  durationMs <- combatVisualLockDurationMs visual
  battleActionLockRemaining %= max (fromIntegral durationMs / 1000)

combatVisualLockDurationMs :: CombatVisualHint -> Combat Int
combatVisualLockDurationMs visual =
  case visual ^. combatVisualDurationMs of
    Just durationMs -> pure durationMs
    Nothing -> combatActionDurationMs $ visual ^. combatVisualActionId

combatActionDurationMs :: Text -> Combat Int
combatActionDurationMs actionId = do
  timingMap <- view combatActionTimings
  pure $ maybe 720 (view combatActionTimingLockMs) (M.lookup actionId timingMap)

combatVisualHint :: Text -> [Text] -> Combat CombatVisualHint
combatVisualHint actionId tags = do
  durationMs <- combatActionDurationMs actionId
  pure
    CombatVisualHint
      { _combatVisualActionId = actionId,
        _combatVisualTags = tags,
        _combatVisualDurationMs = Just durationMs
      }

effectTickVisual :: Text -> Combat CombatVisualHint
effectTickVisual effectKind =
  combatVisualHint ("rig.effect." <> effectKind) ["effect", effectKind]

resolveAnimationRef :: MartialArt -> AnimationRef -> Combat CombatVisualHint
resolveAnimationRef _ animationRef =
  combatVisualHint (animationRef ^. animationRefAction) (animationRef ^. animationRefTags)

battleAttack :: Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
battleAttack left right = do
  attacker <- use $ left . battleChar
  defender <- use $ right . battleChar
  attack <- selectPreparedAttack attacker
  case attack of
    Nothing -> do
      throwError $ CombatException $ pack $ "No unlocked move selected, prepared arts: " <> show (attacker ^. charPrepare)
    Just preparedAttack -> runAttackPipeline left right preparedAttack attacker defender

runAttackPipeline :: Lens' Battle BattleState -> Lens' Battle BattleState -> PreparedAttack -> Character -> Character -> Combat ()
runAttackPipeline left right preparedAttack attacker defender = do
  attackerStats <- battleDerivedStats left
  defenderStats <- battleDerivedStats right
  let attackScore = attackPower attackerStats preparedAttack
      dodgeScore = dodgePower defenderStats
      parryScore = parryPower defenderStats
      move = preparedAttackMove preparedAttack
      moveText = move ^. attackMoveMsg
  visual <- resolveAnimationRef (preparedAttackMartialArt preparedAttack) (move ^. attackMoveAnimation)
  hit <- contest attackScore dodgeScore
  uid <- use battleOwner
  attackerName <- use $ left . battleChar . charName
  defenderName <- use $ right . battleChar . charName
  if not hit
    then
      do
        lockBattleForVisual visual
        tell
          [ ( uid,
              combatEventResp
                CombatEventNormal
                attackerName
                defenderName
                (CombatScriptText $ moveText <> "，却被侧身闪避")
                (Just 0)
                Nothing
                CombatDodge
                visual
            )
          ]
    else do
      parryFailed <- contest attackScore parryScore
      if not parryFailed
        then
          do
            lockBattleForVisual visual
            tell
              [ ( uid,
                  combatEventResp
                    CombatEventNormal
                    attackerName
                    defenderName
                    (CombatScriptText $ moveText <> "，被抬手格开")
                    (Just 0)
                    Nothing
                    CombatParry
                    visual
                )
              ]
        else do
          let damage = applyCombatHooks attacker defender preparedAttack $ computeDamage attackerStats defenderStats preparedAttack
          right . battleChar . charHP -= damage
          lockBattleForVisual visual
          tell
            [ ( uid,
                combatEventResp
                  CombatEventNormal
                  attackerName
                  defenderName
                  (CombatScriptText moveText)
                  (Just damage)
                  Nothing
                  CombatHit
                  visual
              )
            ]

selectPreparedAttack :: Character -> Combat (Maybe PreparedAttack)
selectPreparedAttack char = do
  martialArtMap <- view martialArts
  randomSelect $ unlockedPreparedAttacks martialArtMap char

unlockedPreparedAttacks :: M.Map ArtId MartialArt -> Character -> [PreparedAttack]
unlockedPreparedAttacks martialArtMap char =
  [ PreparedAttack artEntity martialArt move
    | artType' <- [Sword, Fist],
      Just artEntity <- [char ^. charPrepare . at artType'],
      Just martialArt <- [M.lookup (artEntity ^. artDef) martialArtMap],
      move <- martialArt ^. artAttackMoves,
      move ^. attackMoveUnlockLevel <= artEntity ^. artLevel
  ]

contest :: Int -> Int -> Combat Bool
contest attack defense
  | attack <= 0 = return False
  | defense <= 0 = return True
  | otherwise = do
      roll <- getRandomR (1, attack + defense)
      return $ roll <= attack

attackPower :: DerivedStats -> PreparedAttack -> Int
attackPower attackerStats preparedAttack =
  max 1 $
    attackerStats ^. dsAttack
      + (attackerStats ^. dsHit)
      + (preparedAttackMove preparedAttack ^. attackMoveDamage) * 4

dodgePower :: DerivedStats -> Int
dodgePower defenderStats =
  max 0 $ defenderStats ^. dsDodge

parryPower :: DerivedStats -> Int
parryPower defenderStats =
  max 0 $ defenderStats ^. dsParry

computeDamage :: DerivedStats -> DerivedStats -> PreparedAttack -> Int
computeDamage attackerStats defenderStats preparedAttack =
  max 1 $
    baseDamage
      + artBonus
      + attackerStats ^. dsDamageBonus
      - defenderStats ^. dsDamageReduction
  where
    baseDamage = preparedAttackMove preparedAttack ^. attackMoveDamage
    artBonus = (preparedAttackArt preparedAttack ^. artLevel) `div` 3

applyCombatHooks :: Character -> Character -> PreparedAttack -> Int -> Int
applyCombatHooks _ _ _ =
  max 1

canUseActiveSkill :: ActiveSkill -> Lens' Battle BattleState -> Combat Bool
canUseActiveSkill activeSkill state = do
  qi <- use $ state . battleQi
  cds <- use $ state . battleActiveSkillCooldowns
  effects <- use $ state . battleEffects

  let hasEnoughQi = qi >= activeSkill ^. activeSkillCost
      isOffCooldown = not $ M.member (activeSkill ^. activeSkillId) cds
      hasRequiredEffects = all (`M.member` effects) (activeSkill ^. activeSkillReqStatus)

  return $ hasEnoughQi && isOffCooldown && hasRequiredEffects

useActiveSkill :: MartialArt -> ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat ()
useActiveSkill martialArt activeSkill caster target = do
  damageAmount <-
    case activeSkill ^. activeSkillTarget of
      Self -> activeSkillDamageAmount activeSkill caster caster
      _ -> activeSkillDamageAmount activeSkill caster target
  healAmount <-
    case activeSkill ^. activeSkillTarget of
      Self -> activeSkillHealAmount activeSkill caster caster
      _ -> activeSkillHealAmount activeSkill caster target

  -- Consume Qi
  caster . battleQi -= activeSkill ^. activeSkillCost

  -- Set cooldown
  caster . battleActiveSkillCooldowns . at (activeSkill ^. activeSkillId) .= Just (activeSkill ^. activeSkillCooldown)

  -- Apply effects based on target type
  case activeSkill ^. activeSkillTarget of
    Single -> applyActiveSkillEffects activeSkill caster target damageAmount healAmount
    Self   -> applyActiveSkillEffects activeSkill caster caster damageAmount healAmount
    All    -> applyActiveSkillEffects activeSkill caster target damageAmount healAmount  -- For now, same as Single

  uid <- use battleOwner
  casterName <- use $ caster . battleChar . charName
  targetName <-
    case activeSkill ^. activeSkillTarget of
      Self -> use $ caster . battleChar . charName
      _ -> use $ target . battleChar . charName
  visual <- resolveAnimationRef martialArt (activeSkill ^. activeSkillAnimation)
  lockBattleForVisual visual
  tell
    [ ( uid,
        combatEventResp
          CombatEventActiveSkill
          casterName
          targetName
          (CombatScriptText $ activeSkill ^. activeSkillMsg)
          damageAmount
          healAmount
          activeSkillResult
          visual
      )
    ]
  where
    activeSkillResult =
      case activeSkill ^. activeSkillDamage of
        Just _ -> CombatHit
        Nothing -> CombatEffect

activeSkillDamageAmount :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat (Maybe Int)
activeSkillDamageAmount activeSkill caster target =
  case activeSkill ^. activeSkillDamage of
    Nothing -> pure Nothing
    Just baseDamage -> do
      casterStats <- battleDerivedStats caster
      targetStats <- battleDerivedStats target
      casterChar <- use $ caster . battleChar
      let artBonus = activeSkillArtLevel activeSkill casterChar `div` 2
          mitigation = (targetStats ^. dsDefense) `div` 8
      pure . Just . max 1 $ baseDamage + artBonus + (casterStats ^. dsDamageBonus) - mitigation

activeSkillHealAmount :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Combat (Maybe Int)
activeSkillHealAmount activeSkill caster _target =
  case activeSkill ^. activeSkillHeal of
    Nothing -> pure Nothing
    Just baseHeal -> do
      casterStats <- battleDerivedStats caster
      pure . Just $ baseHeal + max 0 ((casterStats ^. dsVitality) - 10) `div` 4

activeSkillArtLevel :: ActiveSkill -> Character -> Int
activeSkillArtLevel activeSkill char =
  case requiredLevels <> preparedLevels of
    [] -> 0
    levels -> maximum levels
  where
    requiredLevels = map (`characterKnownArtLevel` char) (activeSkill ^. activeSkillReqArts)
    preparedLevels = map (^. artLevel) . M.elems $ char ^. charPrepare

applyActiveSkillEffects :: ActiveSkill -> Lens' Battle BattleState -> Lens' Battle BattleState -> Maybe Int -> Maybe Int -> Combat ()
applyActiveSkillEffects activeSkill caster target damageAmount healAmount = do
  case damageAmount of
    Just dmg -> do
      target . battleChar . charHP -= dmg
    Nothing -> return ()

  case healAmount of
    Just heal -> do
      maxHp <- (^. dsMaxHp) <$> battleDerivedStats target
      target . battleChar . charHP %= min maxHp . (+ heal)
    Nothing -> return ()

  forM_ (activeSkill ^. activeSkillEffTarget) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    target . battleEffects . at effId .= Just activeEff

  forM_ (activeSkill ^. activeSkillEffSelf) $ \(effId, duration, value) -> do
    let activeEff = ActiveEffect
          { _activeEffectDef = effId,
            _activeEffectRemaining = duration,
            _activeEffectValue = value
          }
    caster . battleEffects . at effId .= Just activeEff
