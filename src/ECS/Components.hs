{-# LANGUAGE TemplateHaskell #-}

module ECS.Components
    ( Acceleration(Acceleration)
    , Dimensions(Dimensions)
    , MovementDirection(Leftward, Rightward)
    , Position(Position)
    , Velocity(Velocity)
    , ax, ay
    , height
    , px, py
    , vx, vy
    , width
    ) where

import Control.Lens (makeLenses)

data Position          =     Position { _px :: Float, _py :: Float }
    deriving (Eq, Ord, Show)
makeLenses ''Position

data Velocity          =     Velocity { _vx :: Float, _vy :: Float }
    deriving (Eq, Ord, Show)
makeLenses ''Velocity

data Acceleration      =     Acceleration { _ax :: Float, _ay :: Float }
    deriving (Eq, Ord, Show)
makeLenses ''Acceleration

data Dimensions        = Dimensions { _width :: Float, _height :: Float }
    deriving (Eq, Ord, Show)
makeLenses ''Dimensions

data MovementDirection = Leftward | Rightward
    deriving (Eq, Ord, Show)
