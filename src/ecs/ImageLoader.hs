module ImageLoader where

import System.Directory
import Data.List
import qualified Data.Trie as T

import Graphics.Gloss.Juicy
    ( loadJuicy)

import Graphics.Gloss.Interface.Pure.Game
    ( Picture (..)
    )

isImage :: FilePath -> Bool
isImage fp = any (flip isSuffixOf fp) [".jpeg", ".jpg", ".png"]

loadImagesFromAssetsDirectory :: IO (T.Trie)
loadImagesFromAssetsDirectory = do
	filepaths <- listDirectory "../assets/images/"
	let imagepaths = filter isImage filepaths
	let keypairs = (\fp -> (,) fp <$> loadJuicy fp) <$> imagepaths
	let trie = T.fromList $ catMaybes keypairs

	return trie


