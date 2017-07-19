module Components where

data Position 		= 	Position { px :: Double, py :: Double }
	deriving (Eq, Ord, Show)

data Velocity 		= 	Velocity { vx :: Double, vy :: Double }
	deriving (Eq, Ord, Show)

data Acceleration 	= 	Acceleration { ax :: Double, ay :: Double }
	deriving (Eq, Ord, Show)

data Dimensions 	= 	Dimensions { width :: Double, height :: Double }
	deriving (Eq, Ord, Show)