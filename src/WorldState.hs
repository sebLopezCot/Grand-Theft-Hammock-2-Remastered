module WorldState
    ( ControlStream(holdingFire, holdingLeftArrow, holdingRightArrow)
    , WorldState (WorldState, controlStream, entities, imageAssets)
    , WorldState.init
    ) where

import Data.IntMap (IntMap)
import qualified Data.Map as M

import ECS.Entities

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    )

data ControlStream = ControlStream {
    holdingLeftArrow :: Bool,
    holdingRightArrow :: Bool,
    holdingFire :: Bool
}

init :: ControlStream
init = ControlStream {
    holdingLeftArrow = False,
    holdingRightArrow = False,
    holdingFire = False
}

data WorldState = WorldState {
    imageAssets :: M.Map String Picture,
    entities :: IntMap Entity,
    controlStream :: ControlStream
}
