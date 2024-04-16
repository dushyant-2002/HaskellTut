{-# LANGUAGE  TupleSections #-}
module MyLib  where


import System.Random
import System.IO.Error

import System.Directory
import Control.Monad



filesInCurrentDirectory :: IO [FilePath]
filesInCurrentDirectory = do
    contents <- getDirectoryContents "."
    filterM doesFileExist contents



fileSizesInCurrentDirectory :: IO [(FilePath,Integer)]
fileSizesInCurrentDirectory = do
    files <- filesInCurrentDirectory
    -- mapM (\fp -> fmap (fp,) (getFileSize fp)) files
    mapM (\fp -> (fp,) <$> getFileSize fp) files

--Catch Error
-- catchIOError :: IO a -> (IO Error -> IO a) -> IO a
--catchIOError (readFile "filename") (\_ -> return "")

--using maybe For error handling
---catchIOEroor (Just <$> readFile "test.txt")(\_ -> return Nothing)
    


