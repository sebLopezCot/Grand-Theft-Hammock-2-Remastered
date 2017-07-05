module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
    ( Color
    , Display(InWindow)
    , Event(EventKey)
    , Key(Char)
    , KeyState(Down, Up)
    , Picture
    , circleSolid
    , makeColor
    , play
    , translate
    )

data Player = Player (Float, Float) (Float, Float) Float

main :: IO ()
main = play display background framerate initial render handle step

display :: Display
display = InWindow "game" (800, 600) (800, 600)

background :: Color
background = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: Player
initial = Player (0, 0) (0, 0) 20

render :: Player -> Picture
render (Player (x, y) _ z) = translate x y $ circleSolid z

handle :: Event -> Player -> Player
handle (EventKey (Char 'w') Down _ _) (Player p (dx, dy) s) = Player p (dx, dy + 80) s
handle (EventKey (Char 'w') Up _ _) (Player p (dx, dy) s) = Player p (dx, dy - 80) s
handle (EventKey (Char 'a') Down _ _) (Player p (dx, dy) s) = Player p (dx - 80, dy) s
handle (EventKey (Char 'a') Up _ _) (Player p (dx, dy) s) = Player p (dx + 80, dy) s
handle (EventKey (Char 's') Down _ _) (Player p (dx, dy) s) = Player p (dx, dy - 80) s
handle (EventKey (Char 's') Up _ _) (Player p (dx, dy) s) = Player p (dx, dy + 80) s
handle (EventKey (Char 'd') Down _ _) (Player p (dx, dy) s) = Player p (dx + 80, dy) s
handle (EventKey (Char 'd') Up _ _) (Player p (dx, dy) s) = Player p (dx - 80, dy) s
handle (EventKey (Char 'q') Down _ _) (Player p v s) = Player p v (s - 5)
handle (EventKey (Char 'e') Down _ _) (Player p v s) = Player p v (s + 5)
handle _ w = w

step :: Float -> Player -> Player
step t (Player (x, y) (dx, dy) s) = Player (x + t * dx, y + t * dy) (dx, dy) s
