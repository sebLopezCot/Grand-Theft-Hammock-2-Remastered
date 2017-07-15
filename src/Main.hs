
module Main (main) where

import Graphics.Gloss.Juicy
    ( loadJuicy)

import Graphics.Gloss.Interface.Pure.Game
    ( Color
    , Display(InWindow)
    , Event(EventKey)
    , Key(SpecialKey)
    , SpecialKey(KeyUp, KeyDown)
    , KeyState(Down)
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

renderTitle :: Picture
renderTitle = translate (-340) (120) $ scale 0.75 0.75 $ text "GTH"

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
    Pictures (
                  renderBackground p 
                : renderTitle 
                : renderListSelector s 
                : (renderListItem <$> zip items [0..])
            )

handle :: Event -> WorldState -> WorldState
handle (EventKey (SpecialKey KeyDown) Down _ _) 
                 (WorldState p (MenuList items s)) 
                
                = WorldState p (MenuList items ((s+1) `mod` length items))

handle (EventKey (SpecialKey KeyUp) Down _ _) 
                 (WorldState p (MenuList items s)) 
                
                = WorldState p (MenuList items ((s-1) `mod` length items))
handle _ ws = ws

step :: Float -> WorldState -> WorldState
step _ ws = ws

