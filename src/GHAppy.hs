{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module GHAppy where

import Control.Arrow (Arrow (second))
import Control.Exception (catch, throwIO)
import Control.Lens hiding ((<.>))
import Control.Lens.Internal.Coerce (coerce)
import Control.Monad.Freer (
  Eff,
  LastMember,
  Member,
  Members,
  interpret,
  raise,
  reinterpret,
  reinterpret3,
  runM,
  send,
  sendM,
  translate,
 )
import Control.Monad.Freer.Reader (Reader, ask, asks, runReader)
import Control.Monad.Freer.State (State, evalState, execState, get, modify, put, runState)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Freer.Writer
import Control.Monad.Writer (unless)
import Data.Aeson (Value (String))
import qualified Data.Aeson.KeyMap as Map
import Data.Aeson.Lens (
  AsNumber (_Integer),
  AsPrimitive (_String),
  AsValue (_Array, _Object),
  key,
  values,
 )
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString)
import Data.Functor.Contravariant
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Request (requestHeaders))
import Network.HTTP.Simple (
  getResponseBody,
  httpBS,
  parseRequest,
 )
import System.Directory (createDirectory)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (filename)
import System.IO.Error (isAlreadyExistsError)
import System.Process.Typed (ExitCode, proc, runProcess)
import Unsafe.Coerce (unsafeCoerce)
import Text.Pandoc.App 

-- | Used by GHAppy for necessary information.
data Settings = Settings
  { apiKey :: String
  , outputDirectory :: String
  , outputFile :: String
  , repository :: String
  , userAgent :: String
  , pandocTemplateUrl :: String
  , preambleLocation :: FilePath
  }
  deriving (Show, Eq)

type IssueN = Integer

-- | Issue is the standard document GHAppy works with.
data Issue = Issue
  { content :: String
  , number :: Integer
  , title :: String
  , labels :: [String]
  }
  deriving (Show, Eq)

-- | Leaf is used in composition as an element of the final output.
data Leaf = Leaf
  { issueN :: Maybe IssueN
  , preamble :: [String]
  , level :: Integer
  }

newtype Issues = Issues {unIssues :: Map IssueN Issue}
  deriving newtype (Monoid, Semigroup)
  deriving (Show, Eq)

data GHAppyAct a where
  SetUpDir :: GHAppyAct ()
  PullIssues :: GHAppyAct Issues
  SaveAvailableIssues :: GHAppyAct [FilePath]
  GeneratePDF :: [Leaf] -> GHAppyAct ()
makeEffect ''GHAppyAct

data Composer a where
  -- | Adds the Issue with a specific number to the composer.
  AddFile :: IssueN -> Composer ()
  -- | Adds an empty page for the 
  AddNewPage :: Composer ()

  AddAllPagesThat :: Integer -> Predicate Issue -> Composer ()
  AddHeader :: Integer -> String -> Composer ()
makeEffect ''Composer

runCompose :: forall effs a. (Member (State Issues) effs) => Eff (Composer ': effs) a -> Eff effs [Leaf]
runCompose = fmap snd . runCompose'

runCompose' :: forall effs a. (Member (State Issues) effs) => Eff (Composer ': effs) a -> Eff effs (a, [Leaf])
runCompose' = runWriter . compose

compose :: forall effs a. Member (State Issues) effs => Eff (Composer ': effs) a -> Eff (Writer [Leaf] ': effs) a
compose = reinterpret go
  where
    go :: forall effs a. Member (State Issues) effs => Composer a -> Eff (Writer [Leaf] ': effs) a
    go = \case
      AddFile no -> tell [emptyLeaf {issueN = Just no}]
      AddNewPage -> tell [emptyLeaf {preamble = ["\\newpage"]}]
      AddAllPagesThat lvl p ->
        let f = fmap ((\n -> emptyLeaf {issueN = n, level = lvl}) . Just . fst) . M.toList . M.filter (getPredicate p) . unIssues
         in (get >>= tell . f)
      AddHeader lvl str -> tell [emptyLeaf {preamble = [replicate (fromEnum lvl) '#' <> " " <> str]}]

    emptyLeaf = Leaf {issueN = Nothing, preamble = mempty, level = 0}

hasLabel :: String -> Predicate Issue
hasLabel s = Predicate $ \Issue {..} -> s `elem` labels

hasOnlyLabel :: String -> Predicate Issue
hasOnlyLabel s = Predicate $ \Issue {..} -> s `elem` labels && (length labels == 1)

runGHAppy :: Settings -> Eff '[GHAppyAct, Reader Settings, State Issues, IO] a -> IO a
runGHAppy s m = runM (evalState mempty (runReader s (transformGHAppy m)))
  where
    transformGHAppy :: Eff '[GHAppyAct, Reader Settings, State Issues, IO] a -> Eff '[Reader Settings, State Issues, IO] a
    transformGHAppy = interpret go

    go :: GHAppyAct a -> Eff '[Reader Settings, State Issues, IO] a
    go = \case
      SetUpDir -> createOutDirectory
      PullIssues -> pullIssuesImpl
      SaveAvailableIssues -> saveAllIssues
      GeneratePDF ls -> runPandoc ls

createOutDirectory :: Members '[Reader Settings, IO] fs => Eff fs ()
createOutDirectory = do
  x <- asks outputDirectory
  send $ createDirectory x `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)

filePath :: Member (Reader Settings) fs => Issue -> Eff fs (FilePath, Issue)
filePath i@Issue {..} = do
  fileName <- asks ((\f -> "." </> f </> show number <.> "md") . outputDirectory)
  pure (fileName, i)

-- | GHAppy saves the issue to the `outputDirectory`.
saveIssue :: Members '[Reader Settings, IO] fs => Issue -> Eff fs FilePath
saveIssue i@Issue {..} = do
  let ticketContent = formatIssue i
  fileName <- fst <$> filePath i
  send $ writeFile fileName ticketContent
  return fileName

formatIssue :: Issue -> String
formatIssue i@Issue {..} = "# " <> title <> "\n\n" <> bumpHeaders 1 content <> "\n"

-- | Saves all the issues available.
saveAllIssues :: Members '[Reader Settings, State Issues, IO] fs => Eff fs [FilePath]
saveAllIssues = get >>= fmap M.elems . traverse saveIssue . unIssues

-- | GHAppy pulls all the issues from the GitHub repository.
pullIssuesImpl ::
  ( Members '[Reader Settings, State Issues, IO] fs
  , LastMember IO fs
  ) =>
  Eff fs Issues
pullIssuesImpl = do
  Settings {..} <- ask

  response <- sendM $ do
    -- Initialise requests
    initReq <- parseRequest $ "https://api.github.com/repos/" <> repository <> "/issues"
    -- Add headers
    let reqIssues =
          initReq
            { requestHeaders =
                [ ("Accept", "application/vnd.github+json")
                , ("Authorization", fromString $ "Bearer " <> apiKey)
                , ("User-Agent", fromString userAgent)
                ]
            }
    getResponseBody <$> httpBS reqIssues

  labels <- do
    let labels = response ^.. values . key "labels" . _Array
    pure $ fmap (V.toList . fmap mconcat) (fmap (\ls -> ls ^.. key "name" . _String . to unpack) <$> labels)

  issues <- do
    let issueBodies = unpack <$> response ^.. values . key "body" . _String
    let issueTitles = unpack <$> response ^.. values . key "title" . _String
    let issueNumbers = response ^.. values . key "number" . _Integer
    let ks = zip (zip3 issueNumbers issueTitles issueBodies) labels
    pure $ (\((n, t, b), l) -> (n, Issue {content = b, number = n, title = t, labels = l})) <$> ks

  modify (\(Issues s) -> Issues $ s `M.union` M.fromList issues)
  get

leafToMDPP :: Member (State Issues) fs => Leaf -> Eff fs String
leafToMDPP Leaf {..} = do
  (Issues s) <- get
  contents <- case issueN of
    Nothing ->
      pure mempty
    Just no -> do
      let content = formatIssue (s M.! no)
      pure $ bumpHeaders level content
  pure $ (unlines preamble) <> contents

bumpHeaders :: Integer -> String -> String
bumpHeaders l xs = do
  l <- f <$> lines xs
  unlines [l]
  where 
    addSym = replicate (fromEnum l) '#'
    f x = case x of 
      '#' : _ ->  addSym <> x
      _ -> x

runPandoc ::
  ( Members
      '[ Reader Settings
       , State Issues
       , IO
       ]
      fs
  , LastMember IO fs
  ) =>
  [Leaf] -> Eff fs ()
runPandoc fs = do
  Settings {..} <- ask
  pTplFl <- getPandocTemplateLocation
  savePandocTemplate
  preamble <- getPreamble
  let tmpFile = "." </> outputDirectory </> "tmp" <.> "md"
  let outFileP = "." </> outputDirectory </> outputFile <.> "pdf"
  outContent <- (preamble <>) . unlines  <$> traverse leafToMDPP fs
  sendM $ writeFile tmpFile outContent
  sendM $ convertWithOpts $ 
    defaultOpts 
    { optFrom = Just $ fromString "markdown"
    , optTo   = Just $ fromString "pdf"
    , optOutputFile = Just outFileP
    , optInputFiles = Just [tmpFile]
    , optTemplate = Just pTplFl
    }

getPreamble :: (Members '[Reader Settings] fs, LastMember IO fs) => Eff fs String
getPreamble = asks preambleLocation >>= sendM . readFile >>= \c -> pure . unlines $ ["---", c, "---"]

getPandocTemplate ::
  ( Members '[Reader Settings] fs
  , LastMember IO fs
  ) =>
  Eff fs ByteString
getPandocTemplate = do
  tUrl <- asks pandocTemplateUrl
  sendM $ do
    initReq <- parseRequest tUrl
    getResponseBody <$> httpBS initReq

savePandocTemplate ::
  ( Members '[Reader Settings] fs
  , LastMember IO fs
  ) =>
  Eff fs ()
savePandocTemplate = do
  outDir <- asks outputDirectory
  content <- getPandocTemplate
  pTplFl <- getPandocTemplateLocation
  sendM $ BS.writeFile pTplFl content

getPandocTemplateLocation :: Members '[Reader Settings] fs => Eff fs FilePath
getPandocTemplateLocation = asks outputDirectory >>= \d -> pure $ "." </> d </> "template" <.> "tpl"
