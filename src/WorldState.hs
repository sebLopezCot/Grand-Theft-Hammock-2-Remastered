module WorldState where

import qualified Data.Map as M

import ECS.Entities

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    )

data WorldState = WorldState {
	imageAssets :: M.Map String Picture,
	entities :: [Entity]
}


