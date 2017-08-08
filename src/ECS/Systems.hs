module ECS.Systems (controllerSystem, physicsSystem, renderSystem, unloadContentSystem) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Control.Lens (_Just, view, (&), (+~), (-~), (.~), (?~), (^.), (^?))
import Control.Monad ((<=<), join)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (tails, find)
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

(.<) :: (a -> b -> a) -> (a -> b -> a) -> (a -> b -> a)
(.<) f g x y = f (g x y) y

getTonyPos :: [E.Entity] -> (Float, Float)
getTonyPos esList = tonyPos
  where 
    maybeTonyPos = join (***) 
        ( <$> join (view E.position <$> find (view E.isTony) esList) ) (view C.px, view C.py)
    tonyPos = fromMaybe (0,0) $ uncurry (liftA2 (,)) maybeTonyPos

-- PHYSICS
-- ========================================================================================
kinematicsUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
kinematicsUpdate dt es = f <$> es
  where
    f e = case (e ^. E.position, e ^. E.velocity) of
        (Just _, Just vel) -> g vel e
        _                    -> e
    g v ke = ke
        & E.movementDirection ?~ bool C.Leftward C.Rightward (v ^. C.vx >= 0)
        & E.position . _Just . C.px +~ dt * v ^. C.vx
        & E.position . _Just . C.py +~ dt * v ^. C.vy

gravityUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
gravityUpdate dt es = updateIf (view E.hasGravity) f <$> es
  where 
    yPos e = sum $ e ^? E.position . _Just . C.py
    -- TODO: Remove position check (gravity should be acting at all times)
    f e = bool id (E.velocity . _Just . C.vy -~ dt * 100 * 981) (yPos e > 0) e

willCollideWithInX :: Float -> E.Entity -> E.Entity -> Bool
willCollideWithInX dt e1 e2 = fromMaybe False $ do
    (left1, right1) <- getLR e1
    (left2, right2) <- getLR e2
    pure $ e1 ^. E.isCollidable && e2 ^. E.isCollidable && not (right1 <= left2 || left1 >= right2)
  where
    getLR e = do
        px <- e ^? E.position . _Just . C.px
        vx <- e ^? E.velocity . _Just . C.vx
        w <- e ^? E.dimensions . _Just . C.width
        let px' = px + vx * dt
        let left1 = px' - 0.5 * w
        let right1 = px' + 0.5 * w
        pure (left1, right1)

willCollideWithInY :: Float -> E.Entity -> E.Entity -> Bool
willCollideWithInY dt e1 e2 = fromMaybe False $ do
    (top1, bottom1) <- getTB e1
    (top2, bottom2) <- getTB e2
    pure $ e1 ^. E.isCollidable && e2 ^. E.isCollidable && not (top1 <= bottom2 || bottom1 >= top2)
  where
    getTB e = do
        py <- e ^? E.position . _Just . C.py
        vy <- e ^? E.velocity . _Just . C.vy
        h <- e ^? E.dimensions . _Just . C.height
        let py' = py + vy * dt
        let top1 = py' + 0.5 * h
        let bottom1 = py' - 0.5 * h
        pure (top1, bottom1)

collisionUpdate :: Float -> IntMap E.Entity -> IntMap E.Entity
collisionUpdate dt es = foldl update es allCollisionPairs
  where
    allCollisionPairs = allUniquePairs $ IM.toList es

    update im pair@((_,e1),(_,e2)) =
        if willCollideWithInX dt e1 e2 && willCollideWithInY dt e1 e2
        then case (any (view E.isTony) [e1, e2],
                   any (view E.isBullet) [e1, e2],
                   any (view E.isCop) [e1, e2],
                   any (view E.isGround) [e1, e2]) of

            (True, False, True, False)  -> bump im pair
            (False, True, True, False)  -> damage im pair
            (True, False, False, True)  -> bump im pair
            _                   -> im
        else im

    -- Bumping into something simply stops motion
    bump im ((i,e1),(j,e2)) = IM.insert j (e2 & E.velocity ?~ C.Velocity 0 0)
                            $ IM.insert i (e1 & E.velocity ?~ C.Velocity 0 0) im

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
updateTonyPlayer es cs = updateIf (view E.isTony) update <$> es
  where 
    update = walkUpdate . bool id jumpUpdate (jumpButton `elem` cs)
    jumpUpdate e = if sum (e ^? E.velocity . _Just . C.vy) /= 100000 
        then e & E.velocity . _Just . C.vy +~ 400
        else e

    walkUpdate e = case (leftArrow `elem` cs, rightArrow `elem` cs) of
        (True,  False) -> e & E.velocity . _Just . C.vx .~ -580
        (False, True)  -> e & E.velocity . _Just . C.vx .~ 580
        _              -> e & E.velocity . _Just . C.vx .~ 0

updateTonyWeapon :: IntMap E.Entity -> Set Key -> IntMap E.Entity
updateTonyWeapon es cs = foldl update es es
  where
    maybeTony = find (view E.isTony) es
    tonyPos = maybeTony ^? _Just . E.position . _Just
    tonyVel = fromMaybe (C.Velocity 0 0) $ maybeTony ^? _Just . E.velocity . _Just
    update elist _ =
        if fireButton `elem` cs
            && not (any (view E.isBullet) elist)
            && isJust tonyPos
        then flip (IM.insert nextId) elist $ E.bullet 
            & E.position .~ (tonyPos & _Just . C.px +~ 74 & _Just . C.py +~ 44)
            & E.velocity ?~ C.Velocity (max 0 (tonyVel ^. C.vx) + 1200) 0
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
lookupPicture e m = flip M.lookup m =<< e ^. E.pictureFilePath

renderSystem :: IntMap E.Entity -> M.Map String Picture -> [Picture]
renderSystem es m = catMaybes $ transform <$> esList
  where
    esList = toList es
    (tx, _) = getTonyPos esList
    transform e = case e ^. E.position of
        Just pos -> translate 
            ((view C.px pos - tx) * boolToFloat ((not . view E.isTony) e)) 
            (view C.py pos - 150) 
            <$> lookupPicture e m
        Nothing  -> lookupPicture e m

    boolToFloat = fromIntegral . fromEnum



-- UNLOADING CONTENT
-- ========================================================================================
unloadContentSystem :: Float -> IntMap E.Entity -> IntMap E.Entity
unloadContentSystem _ es = foldl applyScreenBoundUnload es idxedBullets
  where
    (centerX, _) = getTonyPos $ toList es
    idxedBullets = IM.toList $ IM.filter (view E.isBullet) es
    applyScreenBoundUnload em idxEnt = if outOfScreenBounds idxEnt 
                                            then deleteBullet em idxEnt 
                                            else em
    deleteBullet em (i, _) = IM.delete i em
    bulletPosX e = sum $ e ^? E.position . _Just . C.px
    outOfScreenBounds (_, e) = abs (bulletPosX e - centerX) > (700.0 / 2) -- hardcoded wall for now
