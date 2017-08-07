module ECS.Systems (controllerSystem, physicsSystem, renderSystem, unloadContentSystem) where

import Control.Monad ((<=<), join)
import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Foldable (toList)
import Data.List (tails, find)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S

import Graphics.Gloss.Interface.Pure.Game
    ( Picture
    , translate
    , Event(EventKey)
    , Key
    , KeyState(Up, Down)
    )

import Controls
import qualified ECS.Components as C
import qualified ECS.Entities as E

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

getTonyPos :: [E.Entity] -> (Float, Float)
getTonyPos esList = tonyPos
    where 
        maybeTonyPos = join (***) 
                    ( <$> join (E.position <$> find E.isTony esList) ) (C.px,C.py)
        tonyPos = fromMaybe (0,0) $ uncurry (liftA2 (,)) maybeTonyPos

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

gravityUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
gravityUpdate dt es = (updateIf E.hasGravity f) <$> es
    where 
        yPos e = fromMaybe 0 $ C.py <$> E.position e
        f e = if yPos e > 0 -- TODO: Remove position check (gravity should be acting at all times)
                then updateVelocity 
                        (fromMaybe 0 $ C.vx <$> E.velocity e) 
                        (fromMaybe 0 $ 
                            (subtract $ dt*(100*9.81)) <$> C.vy <$> E.velocity e)
                        e
                else e

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
                 . gravityUpdate dt
                 . collisionUpdate dt


-- PLAYER CONTROL
-- ========================================================================================
updateTonyPlayer :: IntMap E.Entity -> Set Key -> IntMap E.Entity
updateTonyPlayer es cs = updateIf E.isTony update <$> es
    where 
        update = walkUpdate . jumpUpdate
        jumpUpdate e = case (jumpButton `elem` cs) of
                    True -> 
                            if (fromMaybe 0 $ C.vy <$> E.velocity e) /= 100000 
                                then
                                    updateVelocity 
                                        (fromMaybe 0 $ C.vx <$> E.velocity e) 
                                        (fromMaybe 0 $ 
                                            (+400) <$> C.vy <$> E.velocity e) e
                                else e
                    _    -> e

        walkUpdate e = case (leftArrow `elem` cs, rightArrow `elem` cs) of
                    (True,  False) -> updateVelocity 
                                        (-580) 
                                        (fromMaybe 0 $ C.vy <$> E.velocity e)
                                        e

                    (False, True)  -> updateVelocity 
                                        (580) 
                                        (fromMaybe 0 $ C.vy <$> E.velocity e)
                                        e

                    _              -> updateVelocity 
                                        0 
                                        (fromMaybe 0 $ C.vy <$> E.velocity e)
                                        e

updateTonyWeapon :: IntMap E.Entity -> Set Key -> IntMap E.Entity
updateTonyWeapon es cs = foldl update es es
  where
    maybeTony = find E.isTony es
    tonyPos = E.position $ fromMaybe E.empty $ maybeTony
    tonyVel = fromMaybe C.Velocity {C.vx = 0, C.vy = 0} $ 
                E.velocity $ fromMaybe E.empty $ maybeTony
    update elist _ =
        if fireButton `elem` cs
            && not (any E.isBullet elist)
            && isJust tonyPos
        then IM.insert nextId (
            E.bullet 
                ((\tp -> tp { C.px = C.px tp + 74, C.py = C.py tp + 44 }) 
                    <$> tonyPos) 
                (Just C.Velocity {
                    C.vx = max 0 (C.vx tonyVel) + 1200  , 
                    C.vy = 0
                })
        ) elist
        else elist
    nextId = fromMaybe 1 $ (+ 1) . fst . fst <$> IM.maxViewWithKey es

ctrlStreamSystem :: Event -> Set Key -> Set Key
ctrlStreamSystem (EventKey k Down _ _) = S.insert k
ctrlStreamSystem (EventKey k Up _ _) = S.delete k
ctrlStreamSystem _ = id

controllerSystem :: Event -> IntMap E.Entity -> Set Key -> (IntMap E.Entity, Set Key)
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
    (tx, ty) = getTonyPos esList
    transform e = case E.position e of
        Just pos -> translate 
                        ((C.px pos - tx) * boolToFloat ((not . E.isTony) e)) 
                        ((-150) + C.py pos) 
                        <$> lookupPicture e m

        Nothing  -> lookupPicture e m

    boolToFloat = fromIntegral . fromEnum



-- UNLOADING CONTENT
-- ========================================================================================
unloadContentSystem :: Float -> IntMap E.Entity -> IntMap E.Entity
unloadContentSystem dt es = foldl applyScreenBoundUnload es idxedBullets
    where
        (centerX, centerY) = getTonyPos $ toList es
        idxedBullets = IM.toList $ IM.filter E.isBullet es
        applyScreenBoundUnload em idxEnt = if outOfScreenBounds idxEnt 
                                                then deleteBullet em idxEnt 
                                                else em
        deleteBullet em (i,e) = IM.delete i em
        bulletPosX e = fromMaybe 0 $ C.px <$> E.position e
        outOfScreenBounds (i,e) = abs (bulletPosX e - centerX) > (700.0 / 2) -- hardcoded wall for now


