module Main where

import Control.Monad.Freer
import Control.Monad.Reader (void)
import GHAppy
import OptParser
import Options.Applicative
import System.FilePath ((<.>), (</>))

main :: IO ()
main = do
  settings <- execParser $ info pSettings fullDesc
  let runner = runGHAppy settings
  void $
    runner $ do
      setUpDir
      pullIssues
      fs' <- runCompose $ do
        addHeader 1 "Contents"
        addAllPagesThat 1 (hasLabel "audit" <> hasLabel "audit-meta")
        addNewPage
        addHeader 1 "Findings"
        addAllPagesThat 1 (hasOnlyLabel "audit")
      generatePDF fs'
