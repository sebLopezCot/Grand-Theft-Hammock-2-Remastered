module ECS.Systems where

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import ECS.Components as C
import qualified ECS.Entities as E

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    , translate
    )

-- RENDERING
-- ========================================================================================
lookupPicture :: E.Entity -> M.Map String Picture -> Maybe Picture
lookupPicture e m = flip M.lookup m =<< E.pictureFilePath e

lookupPictures :: [E.Entity] -> M.Map String Picture -> [Maybe Picture]
lookupPictures [] _ = []
lookupPictures (e:es) m = lookupPicture e m : lookupPictures es m

renderSystem :: [E.Entity] -> M.Map String Picture -> [Picture]
renderSystem es m = catMaybes $ zipWith transform es (lookupPictures es m)
    where transform = \e mp -> case (mp, E.position e) of
                    (Just p, Just pos) ->  Just $ translate (C.px pos) (C.py pos) p
                    (Just p, Nothing)  ->  Just p
                    _                  ->  Nothing
