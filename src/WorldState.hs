
module WorldState where

import Data.Map

type ImageAssets = Map FilePath Picture
type AudioAssets = Nothing

data Difficulty = Easy | Medium | Hard

type MusicEnabled = Bool
type SoundFXEnabled = Bool

data WorldState = WorldState 
                    
                    -- Loadable assets
                    ImageAssets
                    AudioAssets

                    -- Menus
                    MainMenuList
                    PauseMenuList

                    -- Settings
                    Difficulty
                    MusicEnabled
                    SoundFXEnabled

                    -- Scene ID
                    SceneID


