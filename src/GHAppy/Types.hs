{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module GHAppy.Types where

import Data.Aeson
import Data.Text
import GHC.Generics

-- data User = User
--   { user'login :: !Text
--   , user'id :: Integer
--   , node_id :: Text
--   , avatar_url :: Text
--   , gravatar_id :: Text
--   , url :: Text
--   , html_url :: Text
--   , followers_url :: Text
--   } deriving (Show,Eq,Generic)
-- instance FromJSON User
-- instance ToJSON User

-- | Labels (in addition to issue numbers) are the main way of filtering and selecting issues to be included in the final report.
newtype Label = Label
  { name :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Label
instance ToJSON Label

-- | An Entry is a GH Issue. We do not require all the information for the purpose of GHAppy, rather only a subset.
data Entry = Entry
  { --url :: Text
    -- , repository_url :: !Text
    -- , labels_url :: !Text
    -- , comments_url :: !Text
    -- , events_url :: !Text
    -- , html_url :: !Text
    -- , id :: Integer
    -- , node_id :: !Text
    number :: Integer
  , title :: Text
  , -- , user :: Object
    labels :: [Label]
  , state :: String
  , -- , locked :: Bool
    -- , assignee :: Text
    -- , assignees :: [Object]
    -- , milestone :: Maybe Text
    -- , comments :: Integer
    -- , created_at :: Text
    -- , updated_at :: Text
    -- , closed_at :: Maybe Text
    -- , author_association :: Text
    -- , active_lock_reason :: Maybe Text
    body :: Maybe Text
    -- , reactions :: Object
    -- , timeline_url :: Maybe Text
    -- , performed_via_github_app :: Maybe Text
    -- , state_reason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Entry
instance ToJSON Entry
