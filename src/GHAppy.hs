{- | 'GHappy' is a library meant to ease the creation of Audit Reports via the
 use of Git Hub Issues. The workflow can be described as follows:

1. Fork the audited repository OR work directly in the repository itself.
2. Label the issues found with suitable and distinct labels.
3. Compose your audit report referencing the issues' numbers or labels.
4. Generate the report.

__Example:__

@
module Main (main) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import GHAppy
import GHAppy.OptParser (gHAppyOpt)
import System.FilePath ((\<.\>))

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
      let logo = \"MLabs-logo-cropped\" \<.\> \"jpg\"
      let logoCropped = \"MLabs-logo\" \<.\> \"jpg\"
      let linkedFiles = \"https:\/\/raw.githubusercontent.com\/mlabs-haskell\/audit-report-template\/master\/linked-files\/images\/\"
      getLinkedFile ImagesDir logo (linkedFiles \<\> logo)
      getLinkedFile ImagesDir logoCropped (linkedFiles \<\> logoCropped)

      -- Download all the issues
      pullIssues

      -- Compose our report
      s <- runCompose auditReport

      -- Generate our pdf.
      generatePDF s

-- | Example Audit report structure.
auditReport :: (Member Composer effs) =\> Eff effs ()
auditReport = do
  addRawMd 0 \"https:\/\/raw.githubusercontent.com\/mlabs-haskell\/audit-report-template\/master\/linked-files\/disclaimer.md\"

  addHeader 1 \"Contents\"
  addFile 1 1
  addFile 1 6
  addNewPage

  addHeader 1 \"Reviews\"

  addHeader 2 \"Not Considered Fixed\"
  addAllPagesThat 2 $ hasLabel \"audit\" \<\> hasLabel \"not-fixed\" \<\> isOpen
  addNewPage

  addHeader 2 \"Considered Fixed\"
  addAllPagesThat 2 $ hasLabel \"audit\" \<\> hasLabel \"fixed\" \<\> isOpen
  addNewPage

  addHeader 2 \"Recommendations\"
  addAllPagesThat 2 $ hasLabel \"audit\" \<\> hasLabel \"recommendation\" \<\> isOpen
  addNewPage

  addHeader 1 \"Appendix\"
  addRawMd 1 \"https:\/\/raw.githubusercontent.com\/mlabs-haskell\/audit-report-template\/master\/linked-files\/vulnerability-types.md\"

@

As can be hopefully seen, one does all the Composing work in the 'Composer' and
hands over to 'GHAppy' to do all the grunt work of retrieving information,
rendering, etc.
-}
module GHAppy (
  -- * Running GHAppy.

  -- | GHAppy is meant to be easy to work with, and these two functions
  --       together with the exposed API should be all you need to come up with a nice
  --       report.
  runGHAppy,
  runCompose,

  -- * The parameters of GHAppy.
  Settings (..),

  -- * GHAppy Effect, and Effect Stacks.
  GHStack,
  Logger (..),
  GHAppyAct (..),
  Composer (..),
  Issue,
  EffGH,

  -- * GHappy specific types.
  Issues,
  IssueN,
  Leaf (..),
  Location (..),

  -- * GHAppy's logistical API.
  setUpDirs,
  pullIssues,
  saveAvailableIssues,
  generatePDF,
  getLinkedFile,

  -- * Composer specific API.
  addAllPagesThat,
  addNewPage,
  addHeader,
  addFile,
  addRawMd,

  -- * GHAppy predicates.

  -- | 'Predicate's are an easy way to create filters for the issues you would want to include in the report.
  -- It is the recommended way to batch the retrieval and inclusion of 'Issue's, rather than the inclusion by
  -- number. The concatenation 'Predicate's provides an intuitive way of composing them.
  hasOnlyLabel,
  hasLabel,
  isOpen,

  -- * Logger API.
  logS,
) where

import Control.Exception (catch, throwIO)
import Control.Monad.Freer (
  Eff,
  LastMember,
  Member,
  Members,
  interpret,
  reinterpret,
  runM,
  send,
  sendM,
 )
import Control.Monad.Freer.Reader (Reader, ask, asks, runReader)
import Control.Monad.Freer.State (State, evalState, get, modify)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Control.Monad.Writer (unless)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString)
import qualified Data.ByteString.Lazy.Internal as LB
import Data.Char (isDigit)
import Data.Functor.Contravariant (Predicate (Predicate), getPredicate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text.Encoding as ENC
import Katip (LogEnv, Namespace (Namespace), Severity (InfoS), logMsg, logStr, runKatipT)

import Data.Maybe (fromMaybe)

import qualified Data.ByteString as B
import Network.HTTP.Conduit (Request (requestHeaders), setQueryString)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

import System.Directory (createDirectory, removeFile)
import System.FilePath ((<.>), (</>))
import System.IO.Error (isAlreadyExistsError)

import qualified GHAppy.Types as T

import Text.Pandoc.App (convertWithOpts, defaultOpts, optFrom, optInputFiles, optOutputFile, optTemplate, optTo)

-- | Used by GHAppy for necessary information.
data Settings = Settings
  { apiKey :: String
  , outputDirectory :: FilePath
  , linkedFilesDirectory :: FilePath
  , imagesDirectory :: FilePath
  , outputFile :: String
  , repository :: String
  , userAgent :: String
  , pandocTemplateUrl :: String
  , preambleLocation :: FilePath
  , logEnvironment :: LogEnv
  }

-- | Places where things are saved to.
data Location
  = -- | Linked files directory.
    LinkedFilesDir
  | -- | Images directory.
    ImagesDir
  | -- | Output directory
    OutputDir
  deriving stock (Show, Eq, Ord)

-- | The number of an Issue, as found on GitHub.
type IssueN = Integer

data Status = Open | Closed
  deriving stock (Show, Eq)

-- | Issue is the standard document GHAppy works with. It is a simpler version of `Entry` and the reasoning behind decoupling the two is both a matter of history and convenience. In future versions this could be replaced with Entry directly.
data Issue = Issue
  { content :: String
  , number :: Integer
  , title :: String
  , labels :: [String]
  , status :: Status
  }
  deriving stock (Show, Eq)

-- | A 'Leaf' is to compose the final output.
data Leaf = Leaf
  { -- | Leafs can contain an 'IssueN' - in which case the content of the 'Issue' is included in
    -- the final output. Alternatively they can simply be in-place text, in which case only
    -- the 'Leaf''s 'preamble' is included.
    issueN :: Maybe IssueN
  , -- | The Preamble is prepended to an included leaf. We use this to either prepend some custom
    -- text to an issue, or to create a non issue entry.
    preamble :: [String]
  , -- | The level of a leaf represents by how many levels we will bump the headers it contains.
    -- '0' means none.
    level :: Integer
  }
  deriving stock (Show, Eq)

{- | A wrapper around a map of IssueN and Issue. It is used for the in-memory
 representation of Issues.
-}
newtype Issues = Issues {unIssues :: Map IssueN Issue}
  deriving newtype (Monoid, Semigroup)
  deriving stock (Show, Eq)

-- | Logger is an 'Eff' that allows us to log, in the default implementation Katip is used for structured logging.
data Logger a where
  -- | Standard Info Log.
  LogS ::
    -- | Log Environment
    [String] ->
    -- | Log Message
    String ->
    -- | Log Effect
    Logger ()

makeEffect ''Logger

-- | GHAppy API.
data GHAppyAct a where
  -- | Creates a directory as specified by 'Settings'.
  SetUpDirs :: GHAppyAct ()
  -- | Pulls all the Issues from the repository specified in 'Settings', and keeps them in memory.
  PullIssues :: GHAppyAct Issues
  -- | Dumps all the issues from memory into a specific folder.
  SaveAvailableIssues :: GHAppyAct [FilePath]
  -- | Uses Pandoc to generate the final report.
  GeneratePDF :: [Leaf] -> GHAppyAct ()
  -- | Get files to be linked by Pandoc.
  GetLinkedFile :: Location -> FilePath -> String -> GHAppyAct ()

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
  -- | Add raw markdown file from GitHub raw file url, at a specific level.
  AddRawMd :: Integer -> String -> Composer ()

makeEffect ''Composer

-- | The usual 'Eff' Stack for GHAppy.
type GHStack = '[Logger, Reader Settings, State Issues, IO]

-- | A constraint that includes the 'GHSTack' and has 'IO' as the last 'Eff'.
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
      AddRawMd lvl url -> tellJust lvl =<< makeRequestUTF8 url
    tellJust l s = tell [emptyLeaf {preamble = [s], level = l}]

    emptyLeaf :: Leaf
    emptyLeaf = Leaf {issueN = Nothing, preamble = mempty, level = 0}

runGHAppy :: Settings -> Eff (GHAppyAct ': GHStack) a -> IO a
runGHAppy s m = runM (evalState mempty (runReader s (runLogger (transformGHAppy m))))
  where
    transformGHAppy :: Eff (GHAppyAct ': GHStack) a -> Eff GHStack a
    transformGHAppy = interpret go

    go :: GHAppyAct a -> Eff GHStack a
    go = \case
      SetUpDirs -> log "SetUpDirs" "Setting up Directories." >> createDirs
      PullIssues -> log "PullIssues" "Pulling Issues from GitHub." >> pullIssuesImpl
      SaveAvailableIssues -> log "SaveAvailableIssues" "Saving all Issues to output directory." >> saveAllIssues
      GeneratePDF ls -> log "GeneratePDF" "Running Pandoc." >> runPandoc ls
      GetLinkedFile location name url -> log "GetLinkedFile" ("Downloading: " <> name <> ".") >> runGetLinked location name url

    log :: String -> String -> Eff GHStack ()
    log s = logS ["runGHAppy", s]

runGetLinked :: Location -> String -> String -> Eff GHStack ()
runGetLinked location name url = do
  contents <- makeRequestBS url
  path <- case location of
    OutputDir -> asks outputDirectory
    LinkedFilesDir -> asks linkedFilesDirectory
    ImagesDir -> asks imagesDirectory
  sendM $ B.writeFile (path </> name) contents

runLogger :: (Members '[Reader Settings, IO] effs, LastMember IO effs) => Eff (Logger ': effs) a -> Eff effs a
runLogger = interpret go
  where
    go :: (Members '[Reader Settings, IO] effs, LastMember IO effs) => Logger a -> Eff effs a
    go = \case
      LogS env msg -> do
        lEnv <- asks logEnvironment
        sendM $ runKatipT lEnv $ logMsg (Namespace $ fromString <$> env) InfoS (logStr msg)

-- | Creates all the necessary directories
createDirs :: Members '[Reader Settings, IO] effs => Eff effs ()
createDirs = do
  outDir <- asks outputDirectory
  lfDir <- asks linkedFilesDirectory
  imgDir <- asks imagesDirectory
  send $ createDirectory outDir `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)
  send $ createDirectory lfDir `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)
  send $ createDirectory imgDir `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)

filePath :: Member (Reader Settings) effs => Issue -> Eff effs (FilePath, Issue)
filePath i@Issue {..} = do
  fileName <- asks ((\f -> "." </> f </> show number <.> "md") . outputDirectory)
  pure (fileName, i)

-- | GHAppy saves the issue to the `outputDirectory`.
saveIssue :: Members '[Reader Settings, IO] effs => Issue -> Eff effs FilePath
saveIssue i = do
  let ticketContent = formatIssue i
  fileName <- fst <$> filePath i
  send $ writeFile fileName ticketContent
  return fileName

formatIssue :: Issue -> String
formatIssue Issue {..} = "# " <> title <> "\n\n" <> bumpHeaders 1 content <> "\n"

-- | Saves all the issues available.
saveAllIssues :: Members '[Reader Settings, State Issues, IO] effs => Eff effs [FilePath]
saveAllIssues = get >>= fmap M.elems . traverse saveIssue . unIssues

-- | Makes a GET requests and returns the body as String. The encoding of the ByteString must be UTF8.
makeRequestUTF8 :: LastMember IO eff => String -> Eff eff String
makeRequestUTF8 url = do
  response <- sendM $ do
    initReq <- parseRequest url
    getResponseBody <$> httpBS initReq
  return $ either (error . show) unpack . ENC.decodeUtf8' $ response

-- | Makes a GET requests and save the content to a file. Basically download.
makeRequestBS :: LastMember IO eff => String -> Eff eff ByteString
makeRequestBS url =
  sendM $ do
    initReq <- parseRequest url
    getResponseBody <$> httpBS initReq

-- | GHAppy pulls all the issues from the GitHub repository.
pullIssuesImpl :: forall effs. EffGH effs => Eff effs Issues
pullIssuesImpl = goFromUntil 1 (== mempty) pullPage
  where
    -- Utility function that allows us to pull all the pages.
    goFromUntil :: forall a m. Monad m => Int -> (a -> Bool) -> (Int -> m a) -> m a
    goFromUntil n p m = m n >>= \r -> if p r then pure r else goFromUntil (n + 1) p m

    -- Pulls page 'n' with 100 issues.
    pullPage :: forall effs. EffGH effs => Int -> Eff effs Issues
    pullPage n = do
      logS ["runGHAppy", "pullPage"] $ "Pulling issues from page " <> show n <> "."
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
                    , ("X-GitHub-Api-Version", "2022-11-28")
                    ]
                }

        let reqIssues' =
              setQueryString
                [ ("per_page", Just "100")
                , ("filter", Just "all")
                , ("state", Just "all")
                , ("direction", Just "asc")
                , ("page", Just $ fromString $ show n)
                , ("labels", Just "audit")
                ]
                reqIssues
        getResponseBody <$> httpBS reqIssues'
      case (decode @[T.Entry]) $ LB.packBytes $ BS.unpack response of
        Nothing -> do
          logS ["getResponseBody", "Decode"] "Decoding failed! Here's what I received when pulling the issues:"
          logS ["getResponseBody", "Decode"] $ show $ LB.packBytes $ BS.unpack response
          error "Cannot decode body! Something has gone fundamentally wrong."
        Just entries -> do
          let issues = fmap toIssue entries
          modify (\(Issues s) -> Issues $ s `M.union` M.fromList issues)
          pure $ Issues $ M.fromList issues

    toIssue entry =
      ( T.number entry
      , Issue
          { title = unpack $ T.title entry
          , content = unpack $ fromMaybe "" $ T.body entry
          , labels = (\(T.Label x) -> unpack x) <$> T.labels entry
          , number = T.number entry
          , status = (\case "open" -> Open; _ -> Closed) $ T.state entry
          }
      )

leafToMDPP :: forall effs. Members '[State Issues] effs => Leaf -> Eff effs String
leafToMDPP Leaf {..} = do
  (Issues s) <- get
  formattedContent <- case issueN of
    Nothing ->
      pure mempty
    Just no -> do
      let err = error . ("Cannot find issue: " <>) . show
      let issue = fromMaybe (err no) $ s M.!? no
      let content = unlines preamble <> formatIssue issue
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
  content <- getPandocTemplate
  pTplFl <- getPandocTemplateLocation
  sendM $ BS.writeFile pTplFl content

getPandocTemplateLocation :: Members '[Reader Settings] fs => Eff fs FilePath
getPandocTemplateLocation = asks outputDirectory >>= \d -> pure $ "." </> d </> "template" <.> "tpl"

-- Predicates ------------------------------------------------------------------------

-- | Predicate for issues that have a specific label.
hasLabel :: String -> Predicate Issue
hasLabel s = Predicate $ \Issue {..} -> s `elem` labels

-- | Predicate for open issue.
isOpen :: Predicate Issue
isOpen = Predicate $ \Issue {..} -> status == Open

-- | Predicate for issues that have only a specific label.
hasOnlyLabel :: String -> Predicate Issue
hasOnlyLabel s = Predicate $ \Issue {..} -> s `elem` labels && (length labels == 1)
