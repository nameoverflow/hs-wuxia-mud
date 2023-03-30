{-# LANGUAGE TypeFamilies #-}
module Game.Property
where

import Data.Text

class Configurable a where
  type Config a
  loadConfig :: Config a -> IO (Either Text a)