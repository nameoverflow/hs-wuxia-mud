{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Property
where

import Data.Text
import qualified Data.Map as M
import Game.Entity
import Utils
import Data.Yaml
import qualified Control.Monad
import System.Directory
import System.FilePath
import Data.Maybe
import Relude (toText)

