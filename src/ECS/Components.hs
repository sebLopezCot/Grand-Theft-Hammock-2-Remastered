module ECS.Components where

data Position          =     Position { px :: Float, py :: Float }
    deriving (Eq, Ord, Show)

data Velocity          =     Velocity { vx :: Float, vy :: Float }
    deriving (Eq, Ord, Show)

data Acceleration      =     Acceleration { ax :: Float, ay :: Float }
    deriving (Eq, Ord, Show)

data Dimensions        = Dimensions { width :: Float, height :: Float }
    deriving (Eq, Ord, Show)

data MovementDirection = Leftward | Rightward
    deriving (Eq, Ord, Show)
