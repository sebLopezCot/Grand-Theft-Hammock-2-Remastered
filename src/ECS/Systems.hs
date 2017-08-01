module ECS.Systems (controllerSystem, physicsSystem, renderSystem) where

import Control.Monad ((<=<), join)
import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Foldable (toList)
import Data.List (tails, find)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)

import qualified ECS.Components as C
import qualified ECS.Entities as E
import qualified WorldState as WS

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    , translate
    , Event(EventKey)
    , Key(Char, MouseButton)
    , MouseButton(LeftButton)
    , KeyState(Up, Down)
    )

-- HELPERS
-- ========================================================================================
updateIf :: (a -> Bool) -> (a -> a) -> a -> a
updateIf test f x = if test x then f x else x

allUniquePairs :: [a] -> [(a,a)]
allUniquePairs = (\l -> (,) (head l) <$> tail l) <=< init . tails

updateVelocity :: Float -> Float -> E.Entity -> E.Entity
updateVelocity dx dy e = e {
    E.velocity =
        Just C.Velocity { C.vx = dx,
                          C.vy = dy
                        }
}

(.<) :: (a -> b -> a) -> (a -> b -> a) -> (a -> b -> a)
(.<) f g x y = f (g x y) y

-- PHYSICS
-- ========================================================================================
kinematicsUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
kinematicsUpdate dt es = f <$> es
    where f e = case (E.position e, E.velocity e) of
                (Just pos, Just vel) -> g pos vel e
                _                    -> e

          g p v ke = ke {
                            E.movementDirection = if C.vx v >= 0
                                then Just C.Rightward
                                else Just C.Leftward ,
                            E.position =
                                Just C.Position {
                                    C.px = C.px p + dt * C.vx v,
                                    C.py = C.py p + dt * C.vy v
                                }
                        }

willCollideWith :: Float -> E.Entity -> E.Entity -> Bool
willCollideWith dt e1 e2 = fromMaybe False $ do
    (left1, right1) <- getLR e1
    (left2, right2) <- getLR e2
    pure $ E.isCollidable e1 && E.isCollidable e2 && not (right1 <= left2 || left1 >= right2)
  where
    getLR e = do
        px <- C.px <$> E.position e
        vx <- C.vx <$> E.velocity e
        w <- C.width <$> E.dimensions e
        let px' = px + vx * dt
        let left1 = px' - 0.5 * w
        let right1 = px' + 0.5 * w
        pure (left1, right1)

collisionUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
collisionUpdate dt es = foldl update es allCollisionPairs
    where
        allCollisionPairs = allUniquePairs $ IM.toList es

        update im pair@((_,e1),(_,e2)) =

                if willCollideWith dt e1 e2
                    then case (any E.isTony [e1, e2],
                               any E.isBullet [e1, e2],
                               any E.isCop [e1, e2]) of

                        (True, False, True) -> bump im pair
                        (False, True, True) -> damage im pair
                        _                   -> im

                    else im

        -- Bumping into something simply stops motion
        bump im ((i,e1),(j,e2)) = IM.insert j (updateVelocity 0 0 e2)
                                $ IM.insert i (updateVelocity 0 0 e1) im

        -- Damage implies a projectile being destroyed and a person being hurt
        damage im ((i,_),(j,_)) = IM.delete j
                                $ IM.delete i im


physicsSystem :: Float -> IntMap E.Entity -> IntMap E.Entity
physicsSystem dt = kinematicsUpdate dt
                 . collisionUpdate dt


-- PLAYER CONTROL
-- ========================================================================================
updateTonyPlayer :: IntMap E.Entity -> WS.ControlStream -> IntMap E.Entity
updateTonyPlayer es cs = updateIf E.isTony update <$> es
    where update entity = case (WS.holdingLeftArrow cs, WS.holdingRightArrow cs) of
                    (True,  False) -> updateVelocity (-580) 0 entity
                    (False, True)  -> updateVelocity 580 0 entity
                    _              -> updateVelocity 0 0 entity

updateTonyWeapon :: IntMap E.Entity -> WS.ControlStream -> IntMap E.Entity
updateTonyWeapon es cs = foldl update es es
  where
    tonyPos = E.position $ fromMaybe E.empty $ find E.isTony es
    update elist _ =
        if WS.holdingFire cs
            && not (any E.isBullet elist)
            && isJust tonyPos
        then IM.insert nextId (
            E.bullet tonyPos (Just C.Velocity {
                C.vx = 800, C.vy = 0
            })
        ) elist
        else elist
    nextId = fromMaybe 1 $ (+ 1) . fst . fst <$> IM.maxViewWithKey es

ctrlStreamSystem :: Event -> WS.ControlStream -> WS.ControlStream
ctrlStreamSystem (EventKey k s _ _) cs = case k of
    Char 'a'               -> cs { WS.holdingLeftArrow = toBool s }
    Char 'd'               -> cs { WS.holdingRightArrow = toBool s }
    MouseButton LeftButton -> cs { WS.holdingFire = toBool s }
    _                      -> cs
    where toBool Down = True
          toBool Up = False
ctrlStreamSystem _ cs = cs

controllerSystem :: Event -> IntMap E.Entity -> WS.ControlStream -> (IntMap E.Entity, WS.ControlStream)
controllerSystem ev es cs = (updatedEntities, updatedCtrlStream)
  where
    updatedCtrlStream = ctrlStreamSystem ev cs
    updatedEntities = (updateTonyPlayer
                   .< updateTonyWeapon)
                      es updatedCtrlStream

-- RENDERING
-- ========================================================================================
lookupPicture :: E.Entity -> M.Map String Picture -> Maybe Picture
lookupPicture e m = flip M.lookup m =<< E.pictureFilePath e

renderSystem :: IntMap E.Entity -> M.Map String Picture -> [Picture]
renderSystem es m = catMaybes $ transform <$> esList
  where
    esList = toList es
    maybeTonyPos = join (***) 
                    ( <$> join (E.position <$> find E.isTony esList) ) (C.px,C.py)
    (tx, ty) = fromMaybe (0,0) $ uncurry (liftA2 (,)) maybeTonyPos
    transform e = case E.position e of
        Just pos -> translate 
                        ((C.px pos - tx) * boolToFloat ((not . E.isTony) e)) 
                        ((-150) + C.py pos) 
                        <$> lookupPicture e m
        Nothing  -> lookupPicture e m

    boolToFloat = fromIntegral . fromEnum
