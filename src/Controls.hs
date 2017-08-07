module Controls (fireButton, jumpButton, leftArrow, rightArrow) where

import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, MouseButton)
    , MouseButton(LeftButton)
    )

leftArrow :: Key
leftArrow = Char 'a'

rightArrow :: Key
rightArrow = Char 'd'

jumpButton :: Key
jumpButton = Char 'w'

fireButton :: Key
fireButton = MouseButton LeftButton
