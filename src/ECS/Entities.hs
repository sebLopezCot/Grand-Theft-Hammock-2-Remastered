module ECS.Entities where

import ECS.Components

-- Entity Definition
-- =====================================
data Entity = Entity {
    position :: Maybe Position,
    velocity :: Maybe Velocity,
    acceleration :: Maybe Acceleration,
    movementDirection :: Maybe MovementDirection,

    dimensions :: Maybe Dimensions,

    pictureFilePath :: Maybe FilePath,

    health :: Maybe Int,
    ammoEachReload :: Maybe Int,

    isTony :: Bool,
    isCop :: Bool,
    isCollidable :: Bool,
    isBullet :: Bool,
    isWeapon :: Bool,
    isBackground :: Bool
} deriving (Eq, Ord, Show)

-- Helper starting entities
-- =====================================
empty :: Entity
empty = Entity {
    position = Nothing,
    velocity = Nothing,
    acceleration = Nothing,
    movementDirection = Nothing,
    
    dimensions = Nothing,
    
    pictureFilePath = Nothing,

    health = Nothing,
    ammoEachReload = Nothing,

    isTony = False,
    isCop = False,
    isCollidable = False,
    isBullet = False,
    isWeapon = False,
    isBackground = False
}

startsFromRest :: Entity -> Entity
startsFromRest e = e {
    position = Just Position { px = 0, py = 0 },
    velocity = Just Velocity { vx = 0, vy = 0 },
    acceleration = Just Acceleration { ax = 0, ay = 0 },
    movementDirection = Just Rightward
}

hasDimensions :: Dimensions -> Entity -> Entity
hasDimensions d e = e { dimensions = Just d }

drawnWithPicture :: FilePath -> Entity -> Entity
drawnWithPicture p e = e { pictureFilePath = Just p }

isABackground :: FilePath -> Entity -> Entity
isABackground p e = (drawnWithPicture p $ e) { isBackground = True }

isABasicObject :: FilePath -> Dimensions -> Bool -> Entity -> Entity
isABasicObject p d c e = 
                startsFromRest
              . hasDimensions d
              . drawnWithPicture p
              $ e { isCollidable = c }

isAPerson :: FilePath -> Entity -> Entity
isAPerson p e = isABasicObject p d True e
    where d = Dimensions { width = 50, height = 100 }

isAWeapon :: FilePath -> Entity -> Entity
isAWeapon p e = (isABasicObject p d False e) { isWeapon = True }
    where d = Dimensions { width = 40, height = 20 }

-- Game entities
-- =====================================
tony :: Entity
tony = (isAPerson "tony.png" $ empty) 
        { isTony = True, health = Just 10 }

cop :: Entity
cop = (isAPerson "cop.png" $ empty) 
        { isCop = True, health = Just 3 }

bullet :: Maybe Position -> Maybe Velocity -> Entity
bullet mp mv = (isABasicObject "bullet.png" dims True 
                $ empty) { 
                    position = mp, 
                    velocity = mv, 
                    isBullet = True 
                }
    where dims = Dimensions { width = 20, height = 10 }

revolver :: Entity
revolver = (isAWeapon "revolver.png" $ empty) 
            { ammoEachReload = Just 6 }

miniOozie :: Entity
miniOozie = (isAWeapon "miniOozie.png" $ empty) 
             { ammoEachReload = Just 50 }

hammock :: Entity
hammock = isABasicObject "hammock.png" dims False $ empty
    where dims = Dimensions { width = 100, height = 50 }

baggedHammock :: Entity
baggedHammock = isABasicObject "baggedHammock.png" dims False $ empty
    where dims = Dimensions { width = 20, height = 60 }

beachBackground :: Entity
beachBackground = isABackground "beachBackground.jpg" $ empty
