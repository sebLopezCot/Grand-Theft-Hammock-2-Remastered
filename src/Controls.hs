module Controls (fireButton, leftArrow, rightArrow) where

import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, MouseButton)
    , MouseButton(LeftButton)
    )

leftArrow :: Key
leftArrow = Char 'a'

rightArrow :: Key
rightArrow = Char 'd'

fireButton :: Key
fireButton = MouseButton LeftButton
