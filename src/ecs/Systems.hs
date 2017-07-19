module Systems where

import Data.Maybe (catMaybes)
import qualified Data.Trie as T

import Components
import qualified Entities as E

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    , translate
    )

-- RENDERING
-- ========================================================================================
lookupPicture :: E.Entity -> T.Trie -> Maybe Picture
lookupPicture e t = T.lookup (E.pictureFilePath e) t

lookupPictures :: [E.Entity] -> T.Trie -> [Maybe Picture]
lookupPictures (e:es) t = lookupPicture e t : lookupPictures es t

renderSystem :: [E.Entity] -> T.Trie -> [Picture]
renderSystem es t = catMaybes $ zipWith transform es (lookupPictures es t)
	where transform = \e mp -> case (mp, E.position e) of
								(Just p, Just pos) -> Just $ translate (px pos) (py pos) p
								(Just p, Nothing) ->  Just p
								(Nothing, Nothing) -> Nothing
