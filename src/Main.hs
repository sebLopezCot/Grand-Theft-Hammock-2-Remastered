-- module Main (main) where

-- import Graphics.Gloss.Interface.Pure.Game
--     ( Color
--     , Display(InWindow)
--     , Event(EventKey)
--     , Key(Char)
--     , KeyState(Down, Up)
--     , Picture
--     , circleSolid
--     , makeColor
--     , play
--     , translate
--     )

-- data Player = Player (Float, Float) (Float, Float) Float

-- main :: IO ()
-- main = play display background framerate initial render handle step

-- display :: Display
-- display = InWindow "game" (800, 600) (800, 600)

-- background :: Color
-- background = makeColor 1 1 1 1

-- framerate :: Int
-- framerate = 60

-- initial :: Player
-- initial = Player (0, 0) (0, 0) 20

-- render :: Player -> Picture
-- render (Player (x, y) _ z) = translate x y $ circleSolid z

-- handle :: Event -> Player -> Player
-- handle (EventKey (Char 'w') Down _ _) (Player p (dx, dy) s) = Player p (dx, dy + 80) s
-- handle (EventKey (Char 'w') Up _ _) (Player p (dx, dy) s) = Player p (dx, dy - 80) s
-- handle (EventKey (Char 'a') Down _ _) (Player p (dx, dy) s) = Player p (dx - 80, dy) s
-- handle (EventKey (Char 'a') Up _ _) (Player p (dx, dy) s) = Player p (dx + 80, dy) s
-- handle (EventKey (Char 's') Down _ _) (Player p (dx, dy) s) = Player p (dx, dy - 80) s
-- handle (EventKey (Char 's') Up _ _) (Player p (dx, dy) s) = Player p (dx, dy + 80) s
-- handle (EventKey (Char 'd') Down _ _) (Player p (dx, dy) s) = Player p (dx + 80, dy) s
-- handle (EventKey (Char 'd') Up _ _) (Player p (dx, dy) s) = Player p (dx - 80, dy) s
-- handle (EventKey (Char 'q') Down _ _) (Player p v s) = Player p v (s - 5)
-- handle (EventKey (Char 'e') Down _ _) (Player p v s) = Player p v (s + 5)
-- handle _ w = w

-- step :: Float -> Player -> Player
-- step t (Player (x, y) (dx, dy) s) = Player (x + t * dx, y + t * dy) (dx, dy) s

module Main (main) where

import Data.Maybe

import Graphics.Gloss.Juicy
    ( loadJuicy)

import Graphics.Gloss.Interface.Pure.Game
    ( Color
    , Display(InWindow)
    , Event(EventKey)
    , Key(Char, SpecialKey)
    , SpecialKey(KeyUp, KeyDown)
    , KeyState(Down, Up)
    , Picture (..)
    , text
    , circle
    , makeColor
    , play
    , translate
    , scale
    )

data MenuList = MenuList [String] Int
data WorldState = WorldState Picture MenuList

main :: IO ()
main = do
    background <- loadBackground
    case background of
        Nothing -> error "No background could be loaded."
        Just b -> play display backgroundColor framerate (initial b) render handle step

width :: Int
width = 800

height :: Int
height = 600

display :: Display
display = InWindow "Grand Theft Hammock" (width, height) (width, height)

backgroundColor :: Color
backgroundColor = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: Picture -> WorldState
initial p = WorldState p ml
        where ml = MenuList ["New Game", "Continue", "Options", "Quit"] 0

listItemVerticalPadding :: Float
listItemVerticalPadding = 55

loadBackground :: IO (Maybe Picture)
loadBackground = loadJuicy "assets/images/main-menu-background.jpg"

renderListItem :: (String, Int) -> Picture
renderListItem (item, i) = let verticalPadding = listItemVerticalPadding
                            in 
                            translate (-340) (-verticalPadding*(fromIntegral i)) 
                            $ scale 0.33 0.33 
                            $ text item

renderListSelector :: Int -> Picture
renderListSelector i = translate (-360) (-listItemVerticalPadding*(fromIntegral i)+14)
                            $ circle 10

renderBackground :: Picture -> Picture
renderBackground p = translate 200 0 $ p

render :: WorldState -> Picture
render (WorldState p (MenuList items s)) = 
    Pictures (     [renderBackground p]
                ++ (renderListItem <$> zip items [0..]) 
                ++ [renderListSelector s]   )

handle :: Event -> WorldState -> WorldState
handle (EventKey (SpecialKey KeyDown) Down _ _) (WorldState p (MenuList items s)) 
                = WorldState p (MenuList items 
                    ((s+1) `mod` length items))
handle (EventKey (SpecialKey KeyUp) Down _ _) (WorldState p (MenuList items s)) 
                = WorldState p (MenuList items 
                    ((s-1) `mod` length items))
handle _ ws = ws

step :: Float -> WorldState -> WorldState
step t ws = ws

