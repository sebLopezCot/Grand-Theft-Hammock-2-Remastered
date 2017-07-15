module System where

-- import Entity (Entity)
-- import qualified Entity as Entity
-- import Repository (Repository)
-- import qualified Repository as Repository
-- import Collection (Collection)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data System = System {
      active :: Bool,
      activate :: (System -> System),
      deactivate :: (System -> System),
      execute :: (System -> System) }

-- empty :: System
-- empty = System True (identity) (identity) (identity)