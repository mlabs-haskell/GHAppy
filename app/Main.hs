module Main where

import OptParser
import Control.Monad.Reader ( void )
import GHAppy
import Options.Applicative
import System.FilePath ( (<.>), (</>) )
import Control.Monad.Freer

main :: IO ()
main = do
  settings <- execParser $ info pSettings fullDesc
  let runner = runGHAppy settings
  void $ runner $ do
    setUpDir
    pullIssues
    fs' <- runCompose $ do
      addHeader 1 "Contents"
      addAllPagesThat 0 (hasLabel "audit" <> hasLabel "audit-meta")
      addHeader 1 "Findings"
      addAllPagesThat 1 (hasOnlyLabel "audit")
    generatePDF fs'