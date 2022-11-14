module OptParser where 

import GHAppy
import Options.Applicative
import System.FilePath ( (<.>), (</>) )

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
