module Main where

import Control.Monad (void)
import GHAppy
import GHAppy.OptParser

main :: IO ()
main = do
  settings <- gHAppyOpt
  let runner = runGHAppy settings
  void $
    runner $ do
      setUpDir
      pullIssues
      fs' <- runCompose $ do
        addDisclaimer
        addHeader 1 "Contents"
        addAllPagesThat 1 (hasLabel "audit" <> hasLabel "audit-meta")
        addNewPage
        addHeader 1 "Findings"
        addAllPagesThat 1 (hasOnlyLabel "audit")
        addHeader 1 "Appendix"
        addVulnTypes
      generatePDF fs'
