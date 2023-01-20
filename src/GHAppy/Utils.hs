module GHAppy.Utils (isDirectory, formatChecksum, getAllFiles, repoName, removeRepoPath) where

import Data.List (intersperse)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, (</>))
import Text.Printf (printf)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

isDirectory :: FilePath -> Bool
isDirectory = not . hasExtension . last . wordsWhen (== '/')

formatChecksum :: String -> FilePath -> String
formatChecksum fileHash = printf "%s...%s  %s" (take 4 fileHash) (drop 60 fileHash)

repoName :: String -> String
repoName = last . wordsWhen (== '/')

removeRepoPath :: String -> String
removeRepoPath = mconcat . intersperse "/" . drop 2 . wordsWhen (== '/')

getAllFiles :: [FilePath] -> IO [FilePath]
getAllFiles [] = return []
getAllFiles (f : fs)
  | isDirectory f = listDirectory f >>= (getAllFiles . (fs ++) . map (f </>))
  | otherwise = (f :) <$> getAllFiles fs
