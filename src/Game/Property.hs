{-# LANGUAGE TypeFamilies #-}
module Game.Property
where

import Data.Text
import qualified Data.Map as M
import Game.Entity
import Utils

class Configurable a where
  type Config a
  loadConfig :: Config a -> IO (Either Text a)