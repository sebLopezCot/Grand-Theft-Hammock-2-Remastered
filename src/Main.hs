module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
    ( Color
    , Display(InWindow)
    , Event
    -- , Event(EventKey)
    -- , Key(SpecialKey)
    -- , SpecialKey(KeyUp, KeyDown)
    -- , KeyState(Down)
    , Picture
    , pictures
    -- , text
    -- , circle
    , makeColor
    , play
    -- , translate
    -- , scale
    )

import Control.Lens ((&), (?~))
import qualified Data.IntMap as IM
import qualified Data.Map as M

import qualified ECS.Entities as E
import qualified ECS.Systems as Systems
import qualified ECS.ImageLoader as ImageLoader
import qualified ECS.Components as C
import qualified WorldState as WS

main :: IO ()
main = do
    imgs <- ImageLoader.loadImagesFromAssetsDirectory
    play display backgroundColor framerate (initial imgs) render handle step

width :: Int
width = 800

height :: Int
height = 600

display :: Display
display = InWindow "Grand Theft Hammock 2 Remastered" (width, height) (width, height)

backgroundColor :: Color
backgroundColor = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: M.Map String Picture -> WS.WorldState
initial imgs = WS.WorldState
    { WS.imageAssets = imgs
    , WS.entities = IM.fromList $ zip [0 ..]
        [ E.beachBackground & E.position ?~ C.Position (-3162) 150
        , E.beachBackground & E.position ?~ C.Position (-2108) 150
        , E.beachBackground & E.position ?~ C.Position (-1054) 150
        , E.beachBackground & E.position ?~ C.Position 0 150
        , E.beachBackground & E.position ?~ C.Position 1054 150
        , E.beachBackground & E.position ?~ C.Position 2108 150
        , E.beachBackground & E.position ?~ C.Position 3162 150
        , E.ground
        , E.treeCurveRight & E.position ?~ C.Position (-900) 174
        , E.treeCurveLeft & E.position ?~ C.Position (-500) 174
        , E.tony & E.position ?~ C.Position 0 0
        , E.cop & E.position ?~ C.Position 200 0
        , E.cop & E.position ?~ C.Position 800 0
        , E.cop & E.position ?~ C.Position 1400 0
        ]
    , WS.controlStream = mempty
    }

render :: WS.WorldState -> Picture
render ws = pictures $ Systems.renderSystem (WS.entities ws) (WS.imageAssets ws)

handle :: Event -> WS.WorldState -> WS.WorldState
handle ev ws = ws { WS.entities = fst results, WS.controlStream = snd results }
    where results =
            Systems.controllerSystem ev (WS.entities ws) (WS.controlStream ws)

step :: Float -> WS.WorldState -> WS.WorldState
step dt ws = ws
    { WS.entities = Systems.unloadContentSystem dt
                  . Systems.physicsSystem dt
                  $ WS.entities ws
    }



-- module Main (main) where

-- import Graphics.Gloss.Juicy
--     ( loadJuicy)

-- import Graphics.Gloss.Interface.Pure.Game
--     ( Color
--     , Display(InWindow)
--     , Event(EventKey)
--     , Key(SpecialKey)
--     , SpecialKey(KeyUp, KeyDown)
--     , KeyState(Down)
--     , Picture (..)
--     , text
--     , circle
--     , makeColor
--     , play
--     , translate
--     , scale
--     )

-- data MenuList = MenuList [String] Int
-- data WorldState = WorldState Picture MenuList

-- main :: IO ()
-- main = do
--     background <- loadBackground
--     case background of
--         Nothing -> error "No background could be loaded."
--         Just b -> play display backgroundColor framerate (initial b) render handle step

-- width :: Int
-- width = 800

-- height :: Int
-- height = 600

-- display :: Display
-- display = InWindow "Grand Theft Hammock" (width, height) (width, height)

-- backgroundColor :: Color
-- backgroundColor = makeColor 1 1 1 1

-- framerate :: Int
-- framerate = 60

-- initial :: Picture -> WorldState
-- initial p = WorldState p ml
--         where ml = MenuList ["New Game", "Continue", "Options", "Quit"] 0

-- listItemVerticalPadding :: Float
-- listItemVerticalPadding = 55

-- loadBackground :: IO (Maybe Picture)
-- loadBackground = loadJuicy "assets/images/main-menu-background.jpg"

-- renderTitle :: Picture
-- renderTitle = translate (-340) (120) $ scale 0.75 0.75 $ text "GTH"

-- renderListItem :: (String, Int) -> Picture
-- renderListItem (item, i) = let verticalPadding = listItemVerticalPadding
--                             in
--                             translate (-340) (-verticalPadding*(fromIntegral i))
--                             $ scale 0.33 0.33
--                             $ text item

-- renderListSelector :: Int -> Picture
-- renderListSelector i = translate (-360) (-listItemVerticalPadding*(fromIntegral i)+14)
--                             $ circle 10

-- renderBackground :: Picture -> Picture
-- renderBackground p = translate 200 0 $ p

-- render :: WorldState -> Picture
-- render (WorldState p (MenuList items s)) =
--     Pictures (
--                   renderBackground p
--                 : renderTitle
--                 : renderListSelector s
--                 : (renderListItem <$> zip items [0..])
--             )

-- handle :: Event -> WorldState -> WorldState
-- handle (EventKey (SpecialKey KeyDown) Down _ _)
--                  (WorldState p (MenuList items s))

--                 = WorldState p (MenuList items ((s+1) `mod` length items))

-- handle (EventKey (SpecialKey KeyUp) Down _ _)
--                  (WorldState p (MenuList items s))

--                 = WorldState p (MenuList items ((s-1) `mod` length items))
-- handle _ ws = ws

-- step :: Float -> WorldState -> WorldState
-- step _ ws = ws

