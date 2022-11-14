module Main where

import Control.Monad.Reader ( void )
import GHAppy
import Options.Applicative
import System.FilePath ( (<.>), (</>) )
import Control.Monad.Freer

pSettings :: Parser Settings
pSettings = Settings <$> pApiKey <*> pOutputDirectory <*> pOutputFile <*> pRepository <*> pUserAgent <*> pPandocTemplateUrl <*> pPreambleLocation
  where
    pApiKey = strOption (long "apikey" <> short 'a')
    pOutputDirectory = strOption (long "output" <> short 'o' <> metavar "OUTDIR")
    pOutputFile = strOption (long "outFile" <> short 'f' <> metavar "OUTFILE")
    pRepository = strOption (long "repo" <> short 'r')
    pUserAgent = strOption (long "user" <> short 'u')
    pPandocTemplateUrl =
      strOption (long "pandoc template url" <> short 't')
      <|> pure "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/templates/latex.tpl"
    pPreambleLocation = strOption (long "preamble location" <> short 'p') <|> pure ("." </> "preamble" <.> "yaml")

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