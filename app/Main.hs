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
      s <- runCompose auditReport
      generatePDF s

auditReport = do
  addDisclaimer

  addHeader 1 "Contents"
  addFile 1 1
  addFile 1 6
  addNewPage

  addHeader 1 "Reviews"

  addHeader 2 "Not Considered Fixed"
  addAllPagesThat 2 $ hasLabel "audit" <> hasLabel "not-fixed" <> isOpen
  addNewPage

  addHeader 2 "Considered Fixed"
  addAllPagesThat 2 $ hasLabel "audit" <> hasLabel "fixed" <> isOpen
  addNewPage

  addHeader 2 "Recommendations"
  addAllPagesThat 2 $ hasLabel "audit" <> hasLabel "recommendation" <> isOpen
  addNewPage

  addHeader 1 "Appendix"
  addVulnTypes
