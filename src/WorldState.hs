module WorldState
    ( WorldState (WorldState, controlStream, entities, imageAssets)
    ) where

import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Set (Set)

import ECS.Entities

import Graphics.Gloss.Interface.Pure.Game
    ( Picture
    , Key
    )

data WorldState = WorldState {
    imageAssets :: M.Map String Picture,
    entities :: IntMap Entity,
    controlStream :: Set Key
}
