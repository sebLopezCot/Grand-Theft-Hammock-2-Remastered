module ECS.Systems where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as M

import qualified ECS.Components as C
import qualified ECS.Entities as E
import qualified WorldState as WS

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    , translate
    , Event(EventKey)
    , Key(SpecialKey)
    , SpecialKey(KeyLeft, KeyRight)
    , KeyState(Up, Down)
    )

-- PHYSICS
-- ========================================================================================
kinematicsUpdate :: Float -> [E.Entity] -> [E.Entity]
kinematicsUpdate dt es = f <$> es
    where f = \e -> case (E.position e, E.velocity e) of
                (Just pos, Just vel) -> g pos vel e
                _                    -> e

          g = \p v ke -> ke { 
                            E.movementDirection = if C.vx v >= 0
                                then Just C.Rightward
                                else Just C.Leftward ,
                            E.position = 
                                Just C.Position { 
                                    C.px = (C.px p) + dt*(C.vx v),
                                    C.py = (C.py p) + dt*(C.vy v)
                                }
                        }

physicsSystem :: Float -> [E.Entity] -> [E.Entity]
physicsSystem dt es = kinematicsUpdate dt es


-- PLAYER CONTROL
-- ========================================================================================
updateVelocity :: Float -> Float -> E.Entity -> E.Entity
updateVelocity dx dy e = e { 
    E.velocity = 
        Just C.Velocity { C.vx = dx, 
                          C.vy = dy
                        } 
}

updateAnyPlayers :: [E.Entity] -> WS.ControlStream -> [E.Entity]
updateAnyPlayers [] _ = []
updateAnyPlayers es cs = (\e -> if E.isTony e then update e else e) <$> es
    where update = 
            \entity -> case (WS.holdingLeftArrow cs, WS.holdingRightArrow cs) of
                    (True,  False) -> updateVelocity (-480) 0 entity
                    (False, True)  -> updateVelocity 480 0 entity
                    _              -> updateVelocity 0 0 entity

ctrlStreamSystem :: Event -> WS.ControlStream -> WS.ControlStream
ctrlStreamSystem event cs = case event of
    (EventKey (SpecialKey KeyLeft) Down _ _)  -> cs { WS.holdingLeftArrow = True }

    (EventKey (SpecialKey KeyRight) Down _ _) -> cs { WS.holdingRightArrow = True }

    (EventKey (SpecialKey KeyLeft) Up _ _)    -> cs { WS.holdingLeftArrow = False }

    (EventKey (SpecialKey KeyRight) Up _ _)   -> cs { WS.holdingRightArrow = False }
     
    _                                         -> cs

controllerSystem :: Event -> [E.Entity] -> WS.ControlStream -> 
                        ([E.Entity], WS.ControlStream)
controllerSystem _ [] cs = ([], cs)
controllerSystem ev es cs = (updatedEntities, updatedCtrlStream)
    where 
        updatedCtrlStream = ctrlStreamSystem ev cs
        updatedEntities = updateAnyPlayers es updatedCtrlStream

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
