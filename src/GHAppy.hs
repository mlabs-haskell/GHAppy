module GHAppy (
  runGHAppy,
  runCompose,
  Settings (..),
  setUpDir,
  pullIssues,
  saveAvailableIssues,
  generatePDF,
  addAllPagesThat,
  hasOnlyLabel,
  hasLabel,
  isOpen,
  addNewPage,
  addHeader,
  addFile,
  addDisclaimer,
  addVulnTypes,
  Leaf (..),
) where

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
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.Writer (unless)
import Data.Aeson --(Value (String), decode)
import qualified Data.Aeson.KeyMap as Map
import Data.Aeson.Lens (
  AsNumber (_Integer),
  AsPrimitive (_String),
  AsValue (_Array, _Object),
  key,
  values,
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (ByteString)
import qualified Data.ByteString.Lazy.Internal as LB
import Data.Char (isDigit)
import Data.Functor.Contravariant (Predicate (Predicate), getPredicate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Katip (LogEnv, Namespace (Namespace), Severity (InfoS), logEnvApp, logMsg, logStr, runKatipT)

import Data.Maybe (fromMaybe)

import Network.HTTP.Conduit (Request (requestHeaders), setQueryString)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

import System.Directory (createDirectory, removeFile)
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (filename)
import System.IO.Error (isAlreadyExistsError)
import System.Process.Typed (ExitCode, proc, runProcess)

import qualified GHAppy.Types as T

import Text.Pandoc.App (convertWithOpts, defaultOpts, optFrom, optInputFiles, optOutputFile, optTemplate, optTo)

-- | Used by GHAppy for necessary information.
data Settings = Settings
  { apiKey :: String
  , outputDirectory :: String
  , outputFile :: String
  , repository :: String
  , userAgent :: String
  , pandocTemplateUrl :: String
  , preambleLocation :: FilePath
  , logEnvironment :: LogEnv
  }

type IssueN = Integer

data Status = Open | Closed
  deriving (Show, Eq)

-- | Issue is the standard document GHAppy works with.
data Issue = Issue
  { content :: String
  , number :: Integer
  , title :: String
  , labels :: [String]
  , status :: Status
  }
  deriving (Show, Eq)

-- | Leaf is used in composition as an element of the final output.
data Leaf = Leaf
  { issueN :: Maybe IssueN
  , preamble :: [String]
  , level :: Integer
  }
  deriving (Show, Eq)

{- | A wrapper around a map of IssueN and Issue. It is used for the in-memory
 representation of Issues.
-}
newtype Issues = Issues {unIssues :: Map IssueN Issue}
  deriving newtype (Monoid, Semigroup)
  deriving (Show, Eq)

-- | Logger is an effect that allows us to log.
data Logger a where
  LogS :: [String] -> String -> Logger ()

makeEffect ''Logger

-- | GHAppy API.
data GHAppyAct a where
  SetUpDir :: GHAppyAct ()
  PullIssues :: GHAppyAct Issues
  SaveAvailableIssues :: GHAppyAct [FilePath]
  GeneratePDF :: [Leaf] -> GHAppyAct ()

makeEffect ''GHAppyAct

-- | API for composing documents from issues.
data Composer a where
  -- | Adds the Issue with a specific number to the composer.
  AddFile :: Integer -> IssueN -> Composer ()
  -- | Adds an empty page.
  AddNewPage :: Composer ()
  -- | Adds all the issues that satisfy a predicate, bumping their headers by a
  -- specific amount.
  AddAllPagesThat :: Integer -> Predicate Issue -> Composer ()
  -- | Adds a header at specific level.
  AddHeader :: Integer -> String -> Composer ()
  -- | Adds the standard disclaimer.
  AddDisclaimer :: Composer ()
  -- | Adds the standard
  AddVulnTypes :: Composer ()

makeEffect ''Composer

-- | The usual Effect Stack for the Monad.
type GHStack = '[Logger, Reader Settings, State Issues, IO]

-- | A constraint that includes the GHSTack and IO as the last Effect.
type EffGH a = (Members GHStack a, LastMember IO a)

runCompose :: forall effs a. EffGH effs => Eff (Composer ': effs) a -> Eff effs [Leaf]
runCompose = fmap snd . runCompose'

runCompose' :: forall effs a. EffGH effs => Eff (Composer ': effs) a -> Eff effs (a, [Leaf])
runCompose' = runWriter . compose

compose :: forall effs a. EffGH effs => Eff (Composer ': effs) a -> Eff (Writer [Leaf] ': effs) a
compose = reinterpret go
  where
    go :: forall effs a. EffGH effs => Composer a -> Eff (Writer [Leaf] ': effs) a
    go = \case
      AddFile lvl no -> tell [emptyLeaf {issueN = Just no, level = lvl}]
      AddNewPage -> tell [emptyLeaf {preamble = ["\\newpage"]}]
      AddAllPagesThat lvl p ->
        let f = fmap ((\n -> emptyLeaf {issueN = n, level = lvl}) . Just . fst) . M.toList . M.filter (getPredicate p) . unIssues
         in (get >>= tell . f)
      AddHeader lvl str -> tell [emptyLeaf {preamble = [replicate (fromEnum lvl) '#' <> " " <> str]}]
      AddDisclaimer -> tellJust 0 =<< makeRequest "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/disclaimer.md"
      AddVulnTypes -> tellJust 1 =<< makeRequest "https://raw.githubusercontent.com/mlabs-haskell/audit-report-template/master/linked-files/vulnerability-types.md"

    tellJust l s = tell [emptyLeaf {preamble = [s], level = l}]

    emptyLeaf :: Leaf
    emptyLeaf = Leaf {issueN = Nothing, preamble = mempty, level = 0}

-- | Predicate for issues that have a specific label.
hasLabel :: String -> Predicate Issue
hasLabel s = Predicate $ \Issue {..} -> s `elem` labels

-- | Predicate for open issue.
isOpen :: Predicate Issue
isOpen = Predicate $ \Issue {..} -> status == Open

-- | Predicate for issues that have only a specific label.
hasOnlyLabel :: String -> Predicate Issue
hasOnlyLabel s = Predicate $ \Issue {..} -> s `elem` labels && (length labels == 1)

runGHAppy :: Settings -> Eff (GHAppyAct ': GHStack) a -> IO a
runGHAppy s m = runM (evalState mempty (runReader s (runLogger (transformGHAppy m))))
  where
    transformGHAppy :: Eff (GHAppyAct ': GHStack) a -> Eff GHStack a
    transformGHAppy = interpret go

    go :: GHAppyAct a -> Eff GHStack a
    go = \case
      SetUpDir -> log "Setting up Directory." >> createOutDirectory
      PullIssues -> log "Pulling Issues." >> pullIssuesImpl
      SaveAvailableIssues -> log "Saving all Issues." >> saveAllIssues
      GeneratePDF ls -> log "Running Pandoc." >> runPandoc ls

    log :: String -> Eff GHStack ()
    log = logS ["runGHAppy"]

runLogger :: (Members '[Reader Settings, IO] effs, LastMember IO effs) => Eff (Logger ': effs) a -> Eff effs a
runLogger = interpret go
  where
    go :: (Members '[Reader Settings, IO] effs, LastMember IO effs) => Logger a -> Eff effs a
    go = \case
      LogS env msg -> do
        lEnv <- asks logEnvironment
        let env' = lEnv ^. logEnvApp
        sendM $ runKatipT lEnv $ logMsg (Namespace $ fromString <$> env) InfoS (logStr msg)

createOutDirectory :: Members '[Reader Settings, IO] effs => Eff effs ()
createOutDirectory = do
  x <- asks outputDirectory
  send $ createDirectory x `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)

filePath :: Member (Reader Settings) effs => Issue -> Eff effs (FilePath, Issue)
filePath i@Issue {..} = do
  fileName <- asks ((\f -> "." </> f </> show number <.> "md") . outputDirectory)
  pure (fileName, i)

-- | GHAppy saves the issue to the `outputDirectory`.
saveIssue :: Members '[Reader Settings, IO] effs => Issue -> Eff effs FilePath
saveIssue i@Issue {..} = do
  let ticketContent = formatIssue i
  fileName <- fst <$> filePath i
  send $ writeFile fileName ticketContent
  return fileName

formatIssue :: Issue -> String
formatIssue i@Issue {..} = "# " <> title <> "\n\n" <> bumpHeaders 1 content <> "\n"

-- | Saves all the issues available.
saveAllIssues :: Members '[Reader Settings, State Issues, IO] effs => Eff effs [FilePath]
saveAllIssues = get >>= fmap M.elems . traverse saveIssue . unIssues

-- | Makes a GET requests and returns the body as String.
makeRequest :: LastMember IO eff => String -> Eff eff String
makeRequest url = do
  response <- sendM $ do
    initReq <- parseRequest url
    getResponseBody <$> httpBS initReq
  let str = repairStr . B8.unpack $ response
  return str
  where
    -- UTF8 characters get broken badly by the B8.unpack
    -- All fixed in-place here.
    repairStr = \case
      ('\226' : '\128' : '\153' : xs) -> repairStr $ '\'' : xs
      ('\226' : '\128' : '\152' : xs) -> repairStr $ '"' : xs
      ('\226' : '\128' : '\156' : xs) -> repairStr $ '"' : xs
      ('\226' : '\128' : '\157' : xs) -> repairStr $ '"' : xs
      ('\226' : '\134' : '\144' : xs) -> repairStr $ '←' : xs
      ('\226' : '\128' : '\166' : xs) -> repairStr $ '…' : xs
      (x : xs) -> x : repairStr xs
      [] -> []

-- | GHAppy pulls all the issues from the GitHub repository.
pullIssuesImpl :: forall effs. EffGH effs => Eff effs Issues
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
    -- fixme: pagination is no handled - if you don't see the issue you are
    -- looking for, this needs to be fixed.
    let reqIssues' =
          setQueryString
            [ ("per_page", Just "100")
            , ("filter", Just "all")
            , ("state", Just "all")
            , ("direction", Just "asc")
            , ("page", Just "1")
            , ("labels", Just "audit")
            ]
            reqIssues
    getResponseBody <$> httpBS reqIssues'

  case (decode @[T.Entry]) $ LB.packBytes $ BS.unpack response of
    Nothing -> error "Cannot decode"
    Just entries -> do
      let issues = fmap toIssue entries
      modify (\(Issues s) -> Issues $ s `M.union` M.fromList issues)
      get
  where
    toIssue entry =
      ( T.number entry
      , Issue
          { title = unpack $ T.title entry
          , content = unpack $ fromMaybe "" $ T.body entry
          , labels = (\(T.Label x) -> x) <$> T.labels entry
          , number = T.number entry
          , status = (\case "open" -> Open; _ -> Closed) $ T.state entry
          }
      )

leafToMDPP :: forall effs. Member (State Issues) effs => Leaf -> Eff effs String
leafToMDPP Leaf {..} = do
  (Issues s) <- get
  formattedContent <- case issueN of
    Nothing ->
      pure mempty
    Just no -> do
      let content = unlines preamble <> formatIssue (s M.! no)
      let bumpedContent = bumpHeaders level content
      replaceNumbers bumpedContent

  let bumpedPreamble = bumpHeaders level $ unlines preamble
  pure $ bumpedPreamble <> formattedContent

-- | Replace issue numbers with title of the issue.
replaceNumbers :: forall effs. Member (State Issues) effs => String -> Eff effs String
replaceNumbers = \case
  '#' : xs -> do
    db <- unIssues <$> get
    case slurpNumber xs of
      Just (no, after) -> do
        let issueTitle = title $ db M.! no
        (("_" <> issueTitle <> "_") <>) <$> replaceNumbers after
      Nothing -> ('#' :) <$> replaceNumbers xs
  x : xs -> (x :) <$> replaceNumbers xs
  [] -> pure []
  where
    slurpNumber :: String -> Maybe (Integer, String)
    slurpNumber str = do
      let n = takeWhile isDigit str
      let l = length n
      ( if l > 0
          then
            ( do
                let after = drop l str
                let no = read @Integer n
                Just (no, after)
            )
          else Nothing
        )

bumpHeaders :: Integer -> String -> String
bumpHeaders l xs = do
  l <- f <$> lines xs
  unlines [l]
  where
    addSym = replicate (fromEnum l) '#'
    f x = case x of
      '#' : _ -> addSym <> x
      _ -> x

runPandoc :: forall effs. EffGH effs => [Leaf] -> Eff effs ()
runPandoc fs = do
  Settings {..} <- ask
  pTplFl <- getPandocTemplateLocation
  savePandocTemplate
  preamble <- getPreamble
  let mdFile = "." </> outputDirectory </> outputFile <.> "md"
  let outFileP = "." </> outputDirectory </> outputFile <.> "pdf"
  outContent <- (preamble <>) . unlines <$> traverse leafToMDPP fs
  sendM $ writeFile mdFile outContent
  sendM $
    convertWithOpts $
      defaultOpts
        { optFrom = Just $ fromString "markdown"
        , optTo = Just $ fromString "pdf"
        , optOutputFile = Just outFileP
        , optInputFiles = Just [mdFile]
        , optTemplate = Just pTplFl
        }
  sendM $ removeFile pTplFl

getPreamble :: (Members '[Reader Settings] effs, LastMember IO effs) => Eff effs String
getPreamble = asks preambleLocation >>= sendM . readFile >>= \c -> pure . unlines $ ["---", c, "---"]

getPandocTemplate :: (Members '[Reader Settings] effs, LastMember IO effs) => Eff effs ByteString
getPandocTemplate = do
  tUrl <- asks pandocTemplateUrl
  sendM $ do
    initReq <- parseRequest tUrl
    getResponseBody <$> httpBS initReq

savePandocTemplate :: (Members '[Reader Settings] effs, LastMember IO effs) => Eff effs ()
savePandocTemplate = do
  outDir <- asks outputDirectory
  content <- getPandocTemplate
  pTplFl <- getPandocTemplateLocation
  sendM $ BS.writeFile pTplFl content

getPandocTemplateLocation :: Members '[Reader Settings] fs => Eff fs FilePath
getPandocTemplateLocation = asks outputDirectory >>= \d -> pure $ "." </> d </> "template" <.> "tpl"
