module GHAppy.YamlInterface (runYamlInterface) where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Data.Char
import Data.Foldable
import Data.Functor.Contravariant
import Data.Yaml
import GHAppy (Composer, GHAppyAct, GHStack)
import qualified GHAppy as GH
import GHAppy.Types (CommitHash)
import GHC.Generics

runYamlInterface :: GH.Settings -> FilePath -> IO ()
runYamlInterface settings path = do
  instr <- either (error . show) id <$> decodeFileEither @YamlInterface path
  yamlToInstructions settings instr

yamlToInstructions :: GH.Settings -> YamlInterface -> IO ()
yamlToInstructions settings YamlInterface {..} = do
  let gInstr = traverse_ gHAppyInstrToEff gHAppy
  let comp = traverse_ compInstrToEff composer

  let runner = GH.runGHAppy settings
  void $
    runner $ do
      -- Set up necessary directories.
      GH.setUpDirs
      -- Download additional files required.
      gInstr
      -- Download all the issues
      GH.pullIssues
      -- Compose our report
      s <- GH.runCompose comp
      -- Generate our pdf.
      GH.generatePDF s

compInstrToEff :: (Member Composer effs) => ComposerInstruction -> Eff effs ()
compInstrToEff = \case
  RawMd {..} -> GH.addRawMd level link
  Header {..} -> GH.addHeader level text
  Issue {..} -> GH.addFile level number
  -- fixme: newpage should be extended
  NewPage {} -> GH.addNewPage
  AllIssuesThat {..} -> GH.addAllPagesThat level (mconcat $ filterToPredicate <$> filters)
  CheckSum {..} -> GH.addCheckSumInfo commitHash files

filterToPredicate :: Filter -> Predicate GH.Issue
filterToPredicate = \case
  HasLabel l -> GH.hasLabel l
  IsOpen True -> GH.isOpen
  IsOpen False -> mempty

gHAppyInstrToEff :: GHAppyInstruction -> Eff (GHAppyAct ': GHStack) ()
gHAppyInstrToEff = \case
  GetLinkedFile {..} -> GH.getLinkedFile (toStdLocation location) name gh'link
  where
    toStdLocation :: String -> GH.Location
    toStdLocation s = case toLower <$> s of
      "linkedfiles" -> GH.LinkedFilesDir
      "images" -> GH.ImagesDir
      "output" -> GH.OutputDir
      _ ->
        error $
          unlines
            [ "Unknown location: " <> s
            , "Useable Locations are:"
            , " * LinkedFiles"
            , " * Images"
            , " * Output"
            ]

data YamlInterface = YamlInterface
  { composer :: [ComposerInstruction]
  , gHAppy :: [GHAppyInstruction]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON YamlInterface where
  parseJSON (Object o) =
    YamlInterface
      <$> o .: "Composer"
      <*> o .: "GHAppy"
  parseJSON _ = mempty

data GHAppyInstruction = GetLinkedFile
  { location :: String
  , name :: String
  , gh'link :: String
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GHAppyInstruction where
  parseJSON (Object o) = do
    iO <- o .: "GetLinkedFile"
    GetLinkedFile
      <$> iO .: "location"
      <*> iO .: "name"
      <*> iO .: "link"
  parseJSON _ = mempty

data ComposerInstruction
  = RawMd
      { level :: Integer
      , link :: String
      }
  | Header
      { level :: Integer
      , text :: String
      }
  | Issue
      { level :: Integer
      , number :: Integer
      }
  | NewPage Integer
  | AllIssuesThat
      { level :: Integer
      , filters :: [Filter]
      }
  | CheckSum
      { files :: [FilePath]
      , commitHash :: CommitHash
      }
  deriving stock (Eq, Show, Generic)

instance FromJSON ComposerInstruction where
  parseJSON (Object o) =
    ( do
        innerObject <- o .: "RawMd"
        le <- innerObject .: "level"
        li <- innerObject .: "link"
        pure $ RawMd le li
    )
      <|> ( do
              innerObject <- o .: "Header"
              le <- innerObject .: "level"
              te <- innerObject .: "text"
              pure $
                Header
                  { level = le
                  , text = te
                  }
          )
      <|> ( do
              innerObject <- o .: "Issue"
              le <- innerObject .: "level"
              nu <- innerObject .: "number"
              pure $
                Issue
                  { level = le
                  , number = nu
                  }
          )
      <|> ( do
              NewPage <$> o .: "NewPage"
          )
      <|> ( do
              innerObject <- o .: "AllIssuesThat"
              le <- innerObject .: "level"
              fs <- innerObject .: "filters"
              pure $
                AllIssuesThat
                  { level = le
                  , filters = fs
                  }
          )
      <|> ( do
              innerObject <- o .: "CheckSum"
              files <- innerObject .: "files"
              commitHash <- innerObject .: "commitHash"
              pure $
                CheckSum
                  { files = files
                  , commitHash = commitHash
                  }
          )
  parseJSON _ = mempty

data Filter
  = HasLabel String
  | IsOpen Bool
  deriving stock (Eq, Show, Generic)

instance FromJSON Filter where
  parseJSON (Object o) =
    (HasLabel <$> o .: "hasLabel")
      <|> (IsOpen <$> o .: "isOpen")
  parseJSON _ = mempty

instance ToJSON Filter where
  toJSON = \case
    HasLabel s -> object ["hasLabel" .= s]
    IsOpen b -> object ["isOpen" .= b]
