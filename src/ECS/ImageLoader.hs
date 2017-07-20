module ECS.ImageLoader where

import System.Directory
import Data.List
import qualified Data.Map as M

import Data.Maybe (catMaybes)

import Graphics.Gloss.Juicy
    ( loadJuicy)

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    )

isImage :: FilePath -> Bool
isImage fp = any (flip isSuffixOf fp) [".jpeg", ".jpg", ".png"]

loadImagesFromAssetsDirectory :: IO (M.Map String Picture)
loadImagesFromAssetsDirectory = do
    let dir = "assets/images/"
    filepaths <- listDirectory dir
    let imagepaths = filter isImage filepaths
    keypairs <- traverse (\fp -> ((,) fp <$>) <$> loadJuicy (dir ++ fp)) imagepaths
    let map = M.fromList $ catMaybes keypairs

    pure map


