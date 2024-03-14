module Main (main) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import GHAppy
import GHAppy.OptParser (gHAppyOpt)
import System.FilePath ((<.>))

-- | Example of a main file for running GHAppy.
main :: IO ()
main = do
  settings <- gHAppyOpt
  let runner = runGHAppy settings
  void $
    runner $ do
      -- Set up necessary directories
      setUpDirs
      -- Download additional images required.
      let logo = "MLabs-logo-cropped" <.> "jpg"
      let logoCropped = "MLabs-logo" <.> "jpg"
      let linkedFiles = "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/images/"
      getLinkedFile ImagesDir logo (linkedFiles <> logo)
      getLinkedFile ImagesDir logoCropped (linkedFiles <> logoCropped)
      -- Download all the issues
      pullIssues
      saveAvailableIssues
      -- Compose our report
      s <- runCompose auditReport
      -- Generate our pdf.
      generatePDF s

-- | Example Audit report structure.
auditReport :: (Member Composer effs) => Eff effs ()
auditReport = do
  addRawMd 0 "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/disclaimer.md"

  addHeader 1 "Contents" -- # Contents
  addFile 1 1 Nothing -- # Contents -> ## Contents
  addFile 1 6 Nothing
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
  addRawMd 1 "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/vulnerability-types.md"
