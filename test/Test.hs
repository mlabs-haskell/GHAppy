module Main (main) where

import Control.Monad ()
import GHAppy.OptParser ()
import Test.Tasty
import Test.Tasty.HUnit ()

main :: IO ()
main =
  defaultMain $
    testGroup "GHAppy Tests" []
