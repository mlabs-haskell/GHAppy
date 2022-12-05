module Main (main, auditReport) where

import Control.Monad (void)
import Control.Monad.Freer
import GHAppy
import GHAppy.OptParser

-- | Example of a main file for running GHAppy.
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

-- | Example Audit report structure.
auditReport :: (Member Composer effs) => Eff effs ()
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
