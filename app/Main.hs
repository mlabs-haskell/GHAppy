module Main (main) where

import GHAppy.OptParser (gHAppyOptApp)
import GHAppy.YamlInterface (runYamlInterface)

main :: IO ()
main = uncurry runYamlInterface =<< gHAppyOptApp
