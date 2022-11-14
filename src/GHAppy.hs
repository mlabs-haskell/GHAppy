{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
module GHAppy where

import Control.Monad.Writer ( unless )
import System.FilePath ( (<.>), (</>) )
import System.Directory ( createDirectory )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )
import Network.HTTP.Conduit ( Request(requestHeaders) )
import Data.Aeson.Lens
    ( key, values, AsNumber(_Integer), AsPrimitive(_String), AsValue (_Array, _Object) )
import Control.Lens hiding ((<.>))--( (^..), (^.), (^?),at ) 
import Data.String ( IsString(fromString) )
import Data.Text (unpack)
import Control.Exception ( catch, throwIO )
import System.IO.Error ( isAlreadyExistsError )
import Data.ByteString.Internal (ByteString)
import System.Process.Typed ( proc, runProcess, ExitCode )
import qualified Data.Map as M
import Data.Map (Map)
import System.FilePath.Lens (filename)
import qualified Data.ByteString as BS
import Control.Monad.Freer.TH ( makeEffect )
import Control.Monad.Freer
    ( reinterpret3,
      runM,
      send,
      sendM,
      Eff,
      LastMember,
      Members,
      reinterpret,
      translate,
      Member, interpret, raise )
import Control.Monad.Freer.Reader ( ask, asks, runReader, Reader )
import Control.Monad.Freer.State ( evalState, get, modify, State, runState, execState,put )
import Control.Monad.Freer.Writer
import Unsafe.Coerce (unsafeCoerce)
import Control.Lens.Internal.Coerce (coerce)
import Control.Arrow (Arrow(second))
import Data.Aeson (Value(String))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Contravariant
import Data.List
import qualified Data.Aeson.KeyMap as Map
import GHC.Generics (Generic)

-- | Used by GHAppy for necessary information.  
data Settings
  = Settings
  { apiKey :: String
  , outputDirectory :: String
  , outputFile :: String
  , repository :: String
  , userAgent :: String
  , pandocTemplateUrl :: String
  , preambleLocation :: FilePath
  } deriving (Show, Eq)

type IssueN = Integer

-- | Issue is the standard document GHAppy works with.
data Issue
  = Issue
  { content :: String
  , number :: Integer
  , title :: String
  , labels :: [String]
  } deriving (Show,Eq)

-- | Leaf is used in composition as an element of the final output.
data Leaf
  = Leaf
  { issueN :: Maybe IssueN
  , preamble :: [String]
  , level :: Integer
  }

newtype Issues = Issues {unIssues :: Map IssueN Issue}
  deriving newtype (Monoid, Semigroup)
  deriving (Show,Eq)

data GHAppyAct a where
  SetUpDir :: GHAppyAct ()
  PullIssues :: GHAppyAct Issues
  SaveAvailableIssues :: GHAppyAct [FilePath]
  GeneratePDF :: [Leaf] -> GHAppyAct ExitCode
makeEffect ''GHAppyAct

data Composer a where
  AddFile :: IssueN -> Composer ()
  AddNewPage :: Composer ()
  AddAllPagesThat :: Integer -> Predicate Issue -> Composer ()
  -- BumpLevel :: forall effs a . (Member (State Issues) effs)  => Integer -> Eff (Composer ': effs) a -> Composer a
  AddHeader :: Integer -> String -> Composer ()
makeEffect ''Composer

runCompose :: forall effs a . (Member (State Issues) effs)  => Eff (Composer ': effs) a -> Eff effs [Leaf]
runCompose  = fmap snd . runCompose' 

runCompose' :: forall effs a . (Member (State Issues) effs)  => Eff (Composer ': effs) a -> Eff effs (a, [Leaf])
runCompose' = runWriter . compose

compose :: forall effs a . Member (State Issues) effs  => Eff (Composer ': effs) a -> Eff (Writer [Leaf] ': effs) a
compose = reinterpret go
  where
    go :: forall effs a . Member (State Issues) effs  =>  Composer a -> Eff (Writer [Leaf] ': effs) a
    go = \case
     AddFile no -> tell [emptyLeaf {issueN = Just no}]
     AddNewPage -> tell [emptyLeaf {preamble = ["\\newpage"]}]
     AddAllPagesThat lvl p  ->
      let f = fmap ((\n -> emptyLeaf {issueN = n, level = lvl}). Just . fst) . M.toList . M.filter (getPredicate p) . unIssues in
        (get >>= tell . f)
    --  BumpLevel l m ->  do
    --   (a,w) <- raise $ runCompose'  m
    --   tell $ (\(lf :: Leaf) -> lf{level = level lf + l}) <$> w
    --   return a
     AddHeader lvl str -> tell [emptyLeaf {preamble = [replicate (fromEnum lvl) '#' <> " " <> str]}]

    emptyLeaf = Leaf {issueN = Nothing, preamble = mempty, level = 0}

hasLabel :: String -> Predicate Issue
hasLabel s = Predicate $ \Issue{..} -> s `elem` labels

hasOnlyLabel :: String -> Predicate Issue
hasOnlyLabel s = Predicate $ \Issue{..} -> s `elem` labels && (length labels == 1 ) 

runGHAppy :: Settings ->  Eff '[GHAppyAct , Reader Settings , State Issues, IO ] a -> IO a
runGHAppy s m = runM (evalState mempty (runReader s (transformGHAppy m)))
  where
    transformGHAppy :: Eff '[GHAppyAct , Reader Settings , State Issues, IO ] a -> Eff '[Reader Settings , State Issues, IO ] a
    transformGHAppy = interpret go

    go :: GHAppyAct a -> Eff '[Reader Settings , State Issues, IO ] a
    go = \case
      SetUpDir -> createOutDirectory
      PullIssues -> pullIssuesImpl
      SaveAvailableIssues -> saveAllIssues
      GeneratePDF ls -> runPandoc ls

createOutDirectory :: Members '[Reader Settings, IO ] fs => Eff fs ()
createOutDirectory = do
  x <- asks outputDirectory
  send $ createDirectory x `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)

filePath :: Member (Reader Settings) fs => Issue -> Eff fs (FilePath,Issue)
filePath i@Issue{..} = do
  fileName <- asks ((\f -> "." </> f </> show number <.> "md") . outputDirectory)
  pure (fileName,i)

-- | GHAppy saves the issue to the `outputDirectory`.
saveIssue :: Members '[Reader Settings, IO ] fs => Issue-> Eff fs FilePath
saveIssue i@Issue{..} = do
  let ticketContent = formatIssue i
  fileName <- fst <$> filePath i
  send $ flip writeFile ticketContent $ fileName
  return fileName

formatIssue :: Issue -> String
formatIssue i@Issue{..} = "# " <> title <> "\n\n" <> content <> "\n"

-- | Saves all the issues available.
saveAllIssues :: Members '[Reader Settings , State Issues, IO  ] fs => Eff fs [FilePath]
saveAllIssues = get >>= fmap M.elems . traverse saveIssue . unIssues

-- | GHAppy pulls all the issues from the GitHub repository.
pullIssuesImpl ::  (Members '[Reader Settings , State Issues, IO  ] fs
                   , LastMember IO fs )
                   => Eff fs Issues
pullIssuesImpl = do
  Settings{..} <- ask

  response <- sendM $ do
    -- Initialise requests 
    initReq <- parseRequest $ "https://api.github.com/repos/" <> repository <> "/issues"
    -- Add headers 
    let reqIssues
          = initReq
            { requestHeaders =
                [ ("Accept", "application/vnd.github+json")
                , ("Authorization", fromString $ "Bearer "<> apiKey)
                , ("User-Agent", fromString userAgent)
                ]
            }
    getResponseBody <$> httpBS reqIssues

  labels <- do
    let labels = response ^.. values . key "labels" . _Array
    pure $ fmap (V.toList . fmap mconcat) (fmap (\ls -> ls ^.. key "name"  . _String . to unpack) <$> labels)

  issues <- do
    let issueBodies = unpack <$> response ^.. values . key "body" . _String
    let issueTitles = unpack <$> response ^.. values . key "title" . _String
    let issueNumbers = response ^.. values . key "number" . _Integer
    let ks = zip (zip3 issueNumbers issueTitles issueBodies) labels
    pure $ (\((n,t,b),l)-> (n,Issue {content = b, number = n, title = t, labels = l})) <$> ks

  modify (\(Issues s) -> Issues $ s `M.union` M.fromList issues)
  get

leafToMDPP :: Member (State Issues) fs => Leaf -> Eff fs [String]
leafToMDPP Leaf{..} = do
  (Issues s) <- get
  contents <- case issueN of
      Nothing ->
        pure mempty
      Just no -> do
        let content = [formatIssue (s M.! no)]
        pure $ bumpHeaders level content
  pure $ preamble <> contents

bumpHeaders :: Integer -> [String] -> [String]
bumpHeaders l xs = ns
  where
    xs' = mconcat $ fmap lines xs 
    is = fmap (elemIndex '#') xs'
    sps = mconcat $ (\(i,l) -> case i of Just ind -> [(ind,l)] ; Nothing -> []) <$> zip is xs
    ls = uncurry splitAt <$> sps
    ns = fmap (\(xs,ys) -> xs <> replicate (fromEnum l) '#' <> ys ) ls


runPandoc :: (Members '[ Reader Settings 
                       , State Issues
                       , IO
                       ] fs
             , LastMember IO fs ) => [Leaf] -> Eff fs ExitCode
runPandoc fs = do
  Settings{..} <- ask
  pTplFl <- getPandocTemplateLocation
  savePandocTemplate
  preamble <- getPreamble
  let tmpFile = "." </> outputDirectory </> "tmp" <.> "md"
  let outFileP = "." </> outputDirectory </> outputFile <.> "pdf"
  let pandocSettings = [ "-o", outFileP
                       , "--template=" <> pTplFl
                       ]
  outContent <- (preamble <>) . unlines . mconcat <$> traverse leafToMDPP fs
  sendM $ writeFile tmpFile outContent

  sendM $ runProcess (proc "pandoc" $ pandocSettings <> [tmpFile])

getPreamble :: (Members '[Reader Settings] fs, LastMember IO fs) => Eff fs String
getPreamble = asks preambleLocation >>= sendM . readFile >>= \c -> pure . unlines $ ["---", c , "---"]

getPandocTemplate :: (Members '[Reader Settings] fs
             , LastMember IO fs ) => Eff fs ByteString
getPandocTemplate = do
  tUrl <- asks pandocTemplateUrl
  sendM $ do
    initReq <- parseRequest tUrl
    getResponseBody <$> httpBS initReq

savePandocTemplate :: (Members '[Reader Settings] fs
                      , LastMember IO fs ) => Eff fs ()
savePandocTemplate = do
  outDir <- asks outputDirectory
  content <- getPandocTemplate
  pTplFl <- getPandocTemplateLocation
  sendM $ BS.writeFile pTplFl content

getPandocTemplateLocation :: Members '[Reader Settings] fs => Eff fs FilePath
getPandocTemplateLocation = asks outputDirectory >>= \d -> pure $ "." </> d </> "template" <.> "tpl"
