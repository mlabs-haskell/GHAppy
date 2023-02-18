module GHAppy.YamlInterface (runYamlInterface) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (Predicate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Object),
  decodeFileEither,
  object,
  (.:),
  (.=),
 )
import GHAppy (Composer, GHAppyAct, GHStack)
import qualified GHAppy as GH
import GHC.Generics (Generic)

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

filterToPredicate :: Filter -> Predicate GH.Issue
filterToPredicate = \case
  HasLabel l -> GH.hasLabel l
  IsOpen True -> GH.isOpen
  IsOpen False -> mempty

gHAppyInstrToEff :: GHAppyInstruction -> Eff (GHAppyAct ': GHStack) ()
gHAppyInstrToEff = \case
  GetLinkedFile {..} -> GH.getLinkedFile (toStdLocation location) (T.unpack name) gh'link
  where
    toStdLocation :: Text -> GH.Location
    toStdLocation s = case T.toLower s of
      "linkedfiles" -> GH.LinkedFilesDir
      "images" -> GH.ImagesDir
      "output" -> GH.OutputDir
      _ ->
        error $
          unlines
            [ "Unknown location: " <> T.unpack s
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
  { location :: Text
  , name :: Text
  , gh'link :: Text
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
      , link :: Text
      }
  | Header
      { level :: Integer
      , text :: Text
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
  parseJSON _ = mempty

data Filter
  = HasLabel Text
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
