module ECS.Systems where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as M

import ECS.Components as C
import qualified ECS.Entities as E

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    , translate
    , Event(EventKey)
    , Key(SpecialKey)
    , SpecialKey(KeyLeft, KeyRight)
    , KeyState(Down)
    )

-- PLAYER CONTROL
-- ========================================================================================
updatePosition :: Float -> Float -> E.Entity -> E.Entity
updatePosition dx dy e = e { 
    E.position = Just C.Position { px = fromMaybe 0 $ (+dx) <$> C.px <$> E.position e, 
                                   py = fromMaybe 0 $ (+dy) <$> C.py <$> E.position e
                                 } 
}

controllerSystem :: Event -> [E.Entity] -> [E.Entity]
controllerSystem _ [] = []
controllerSystem event es = (\e -> if E.isTony e then update e else e) <$> es
    where update = \entity -> case event of 
                    (EventKey (SpecialKey KeyLeft) Down _ _)  
                        -> updatePosition (-25) 0 entity
                    (EventKey (SpecialKey KeyRight) Down _ _) 
                        -> updatePosition 25 0 entity
                    _                                         
                        -> entity

-- RENDERING
-- ========================================================================================
lookupPicture :: E.Entity -> M.Map String Picture -> Maybe Picture
lookupPicture e m = flip M.lookup m =<< E.pictureFilePath e

lookupPictures :: [E.Entity] -> M.Map String Picture -> [Maybe Picture]
lookupPictures [] _ = []
lookupPictures (e:es) m = lookupPicture e m : lookupPictures es m

renderSystem :: [E.Entity] -> M.Map String Picture -> [Picture]
renderSystem [] _ = []
renderSystem es m = catMaybes $ zipWith transform es (lookupPictures es m)
    where transform = \e mp -> case (mp, E.position e) of
                    (Just p, Just pos) ->  Just $ translate (C.px pos) (C.py pos) p
                    (Just p, Nothing)  ->  Just p
                    _                  ->  Nothing
