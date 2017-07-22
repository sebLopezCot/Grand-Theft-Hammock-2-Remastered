module ECS.Systems where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.List (tails)
import Control.Monad ((<=<))
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM

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

-- HELPERS
-- ========================================================================================
updateIf :: (a -> Bool) -> (a -> a) -> a -> a
updateIf test f x = if test x then f x else x

updateIfHas :: (a -> Maybe b) -> (a -> a) -> a -> a
updateIfHas query f x = updateIf (isJust . query) f x

allUniquePairs :: [a] -> [(a,a)]
allUniquePairs = (\l -> (,) (head l) <$> tail l) <=< init . tails

entityListToIntMap :: [a] -> IM.IntMap a
entityListToIntMap xs = IM.fromList $ zip [0..] xs

intMapToEntityList :: IM.IntMap a -> [a]
intMapToEntityList im = snd <$> (IM.toList im)

updateVelocity :: Float -> Float -> E.Entity -> E.Entity
updateVelocity dx dy e = e { 
    E.velocity = 
        Just C.Velocity { C.vx = dx, 
                          C.vy = dy
                        } 
}

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

willCollideWith :: Float -> E.Entity -> E.Entity -> Bool
willCollideWith dt e1 e2 = (E.isCollidable e1) && (E.isCollidable e2) 
                                && not (right1 <= left2 || left1 >= right2)
    where 
        lookahead = 1

        px1 = fromMaybe 0 $ C.px <$> E.position e1
        vx1 = fromMaybe 0 $ C.vx <$> E.velocity e1
        w1 = fromMaybe 0 $ C.width <$> E.dimensions e1
        left1 = px1 - 0.5 * w1 + vx1 * dt * lookahead
        right1 = px1 + 0.5 * w1 + vx1 * dt * lookahead

        px2 = fromMaybe 0 $ C.px <$> E.position e2
        vx2 = fromMaybe 0 $ C.vx <$> E.velocity e2
        w2 = fromMaybe 0 $ C.width <$> E.dimensions e2
        left2 = px2 - 0.5 * w2 + vx2 * dt * lookahead
        right2 = px2 + 0.5 * w2 + vx2 * dt * lookahead

collisionUpdate :: Float -> [E.Entity] -> [E.Entity]
collisionUpdate dt es = intMapToEntityList 
                            $ foldl update idxToEntity allCollisionPairs 
    where
        idxToEntity = entityListToIntMap es
        allCollisionPairs = allUniquePairs $ zip [0..] es

        update = \im ((i,e1),(j,e2)) -> 

                if willCollideWith dt e1 e2
                    then
                        IM.insert j (updateVelocity 0 0 e2) 
                            $ IM.insert i (updateVelocity 0 0 e1) im
                        
                    else im

physicsSystem :: Float -> [E.Entity] -> [E.Entity]
physicsSystem dt es = kinematicsUpdate dt 
                    . collisionUpdate dt 
                    $ es


-- PLAYER CONTROL
-- ========================================================================================
updateAnyPlayers :: [E.Entity] -> WS.ControlStream -> [E.Entity]
updateAnyPlayers [] _ = []
updateAnyPlayers es cs = (updateIf E.isTony update) <$> es
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
