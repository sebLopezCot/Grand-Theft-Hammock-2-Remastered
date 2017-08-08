{-# LANGUAGE TemplateHaskell #-}

module ECS.Entities
    ( Entity
    , beachBackground
    , bullet
    , cop
    , dimensions
    , empty
    , hasGravity
    , isBackground
    , isBullet
    , isCollidable
    , isCop
    , isTony
    , isGround
    , movementDirection
    , pictureFilePath
    , position
    , tony
    , ground
    , treeCurveLeft
    , treeCurveRight
    , velocity
    ) where

import Control.Lens (makeLenses, (&), (.~), (?~))

import ECS.Components

-- Entity Definition
-- =====================================
data Entity = Entity
    { _position :: Maybe Position
    , _velocity :: Maybe Velocity
    , _acceleration :: Maybe Acceleration
    , _movementDirection :: Maybe MovementDirection

    , _dimensions :: Maybe Dimensions

    , _pictureFilePath :: Maybe FilePath

    , _health :: Maybe Int
    , _ammoEachReload :: Maybe Int

    , _isTony :: Bool
    , _isCop :: Bool
    , _hasGravity :: Bool
    , _isCollidable :: Bool
    , _isBullet :: Bool
    , _isWeapon :: Bool
    , _isGround :: Bool
    , _isBackground :: Bool
    } deriving (Eq, Ord, Show)
makeLenses ''Entity

-- Helper starting entities
-- =====================================
empty :: Entity
empty = Entity
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing
    False False False False
    False False False False

startsFromRest :: Entity -> Entity
startsFromRest e = e
    & position ?~ Position 0 0
    & velocity ?~ Velocity 0 0
    & acceleration ?~ Acceleration 0 0
    & movementDirection ?~ Rightward

isABackground :: FilePath -> Entity -> Entity
isABackground p e = e
    & pictureFilePath ?~ p
    & isBackground .~ True
    & position ?~ Position 0 0

isABasicObject :: FilePath -> Dimensions -> Bool -> Entity -> Entity
isABasicObject p d c e = e
    & isCollidable .~ c
    & startsFromRest
    & dimensions ?~ d
    & pictureFilePath ?~ p

isAPerson :: FilePath -> Entity -> Entity
isAPerson p e = isABasicObject p d True e & hasGravity .~ True
    where d = Dimensions 50 100

isAWeapon :: FilePath -> Entity -> Entity
isAWeapon p e = isABasicObject p d False e & isWeapon .~ True
    where d = Dimensions 40 20

-- Game entities
-- =====================================
tony :: Entity
tony = isAPerson "tony.png" empty
    & isTony .~ True
    & health ?~ 10

cop :: Entity
cop = isAPerson "cop.png" empty
    & isCop .~ True
    & health ?~ 3

bullet :: Entity
bullet = isABasicObject "bullet.png" dims True empty
    & isBullet .~ True
    where dims = Dimensions 20 10

revolver :: Entity
revolver = isAWeapon "revolver.png" empty
    & ammoEachReload ?~ 6

miniOozie :: Entity
miniOozie = isAWeapon "miniOozie.png" empty
    & ammoEachReload ?~ 50

hammock :: Entity
hammock = isABasicObject "hammock.png" dims False empty
    where dims = Dimensions 100 50

baggedHammock :: Entity
baggedHammock = isABasicObject "baggedHammock.png" dims False empty
    where dims = Dimensions 20 60

ground :: Entity
ground = isABasicObject "" dims True empty
    & isGround .~ True
    & position ?~ Position 0 (-130)
    where dims = Dimensions 600 150

beachBackground :: Entity
beachBackground = isABackground "beachBackgroundLong.jpg" empty

treeCurveLeft :: Entity
treeCurveLeft = isABasicObject "treeCurveLeft.png" dims False empty
    where dims = Dimensions 400 526

treeCurveRight :: Entity
treeCurveRight = isABasicObject "treeCurveRight.png" dims False empty
    where dims = Dimensions 400 526
