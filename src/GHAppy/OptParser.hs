module GHAppy.OptParser (gHAppyOpt, pSettings, gHAppyOptApp) where

import GHAppy
import Katip
import Options.Applicative
import System.FilePath ((<.>), (</>))
import System.IO (stdout)

-- | Retrieves the Settings and initialises the Logger to output to stdout.
gHAppyOptApp :: IO (Settings, FilePath)
gHAppyOptApp = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  mkLogEnv <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "GHAppy" "production"
  (f, fp) <- execParser $ info ((,) <$> pSettings <*> pYamlLocation) fullDesc
  pure (f mkLogEnv, fp)
  where
    pYamlLocation = strOption (long "input" <> short 'i' <> metavar "input file")

-- | Retrieves the Settings and initialises the Logger to output to stdout.
gHAppyOpt :: IO Settings
gHAppyOpt = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  mkLogEnv <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "GHAppy" "production"
  f <- execParser $ info pSettings fullDesc
  pure $ f mkLogEnv

-- | An OptParser for the standard GHAppy settings. In most cases you will want to use 'gHAppyOpt'.
pSettings :: Parser (LogEnv -> Settings)
pSettings =
  Settings
    <$> pApiKey
    <*> pOutputDirectory
    <*> pLinkedFilesDirectory
    <*> pImagesDirectory
    <*> pOutputFile
    <*> pRepository
    <*> pUserAgent
    <*> pPandocTemplateUrl
    <*> pPreambleLocation
  where
    pApiKey = strOption (long "apikey" <> short 'a')
    pOutputDirectory = strOption (long "output" <> short 'o' <> metavar "OUTDIR")
    pLinkedFilesDirectory = strOption (long "linked-files") <|> pure ("." </> "linked-files")
    pImagesDirectory = strOption (long "images-dir") <|> ((</> "images") <$> pLinkedFilesDirectory)
    pOutputFile = strOption (long "outFile" <> short 'f' <> metavar "OUTFILE")
    pRepository = strOption (long "repo" <> short 'r')
    pUserAgent = strOption (long "user" <> short 'u')
    pPandocTemplateUrl =
      strOption (long "pandoc template url" <> short 't')
        <|> pure "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/templates/latex.tpl"
    pPreambleLocation = strOption (long "preamble location" <> short 'p') <|> pure ("." </> "preamble" <.> "yaml")
