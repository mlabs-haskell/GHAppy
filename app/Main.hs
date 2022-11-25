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
        addAllPagesThat 1 (hasLabel "audit" <> hasLabel "audit-meta" <> isOpen)
        addNewPage

        addHeader 1 "Reviews"
        addAllPagesThat 2 (hasLabel "audit" <> hasLabel "audit-meta" <> isOpen)

        addHeader 2 "Not Considered Fixed"
        addAllPagesThat 2 (hasLabel "audit" <> hasLabel "not-fixed" <> isOpen)

        addHeader 2 "Considered Fixed"
        addAllPagesThat 2 (hasLabel "audit" <> hasLabel "fixed" <> isOpen)

        addHeader 2 "Recommendations"
        addAllPagesThat 2 (hasLabel "audit" <> hasLabel "recommendation" <> isOpen)

        addHeader 1 "Appendix"
        addVulnTypes

      generatePDF fs'
