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

main :: IO ()
main = play display background framerate initial render handle step

display :: Display
display = InWindow "Grand Theft Hammock" (800, 600) (800, 600)

background :: Color
background = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: MenuList
initial = MenuList ["New Game", "Continue", "Options", "Quit"] 0

listItemVerticalPadding :: Float
listItemVerticalPadding = 55

renderListItem :: (String, Int) -> Picture
renderListItem (item, i) = let verticalPadding = listItemVerticalPadding
                            in 
                            translate (-300) (-verticalPadding*(fromIntegral i)) 
                            $ scale 0.33 0.33 
                            $ text item

renderListSelector :: Int -> Picture
renderListSelector i = translate (-320) (-listItemVerticalPadding*(fromIntegral i)+14)
                            $ circle 10

render :: MenuList -> Picture
render (MenuList items s) = Pictures ((renderListItem <$> zip items [0..]) ++ [renderListSelector s])

handle :: Event -> MenuList -> MenuList
handle (EventKey (SpecialKey KeyDown) Down _ _) (MenuList items s) = (MenuList items ((s+1) `mod` (length items)))
handle (EventKey (SpecialKey KeyUp) Down _ _) (MenuList items s) = (MenuList items ((s-1) `mod` (length items)))
handle _ w = w

step :: Float -> MenuList -> MenuList
step t w = w

