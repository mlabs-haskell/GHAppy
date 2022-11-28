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

newtype Label = Label
  { name :: String
  }
  deriving (Show, Eq, Generic)
instance FromJSON Label
instance ToJSON Label

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
  deriving (Show, Eq, Generic)
instance FromJSON Entry
instance ToJSON Entry
