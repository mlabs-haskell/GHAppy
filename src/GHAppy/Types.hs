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

{-
{
    "url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1",
    "repository_url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit",
    "labels_url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1/labels{/name}",
    "comments_url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1/comments",
    "events_url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1/events",
    "html_url": "https://github.com/mlabs-haskell/optim-onchain-audit/issues/1",
    "id": 1441777152,
    "node_id": "I_kwDOIZpwYM5V78YA",
    "number": 1,
    "title": "Audit - Meta Task",
    "user": {
      "login": "cstml",
      "id": 29694576,
      "node_id": "MDQ6VXNlcjI5Njk0NTc2",
      "avatar_url": "https://avatars.githubusercontent.com/u/29694576?v=4",
      "gravatar_id": "",
      "url": "https://api.github.com/users/cstml",
      "html_url": "https://github.com/cstml",
      "followers_url": "https://api.github.com/users/cstml/followers",
      "following_url": "https://api.github.com/users/cstml/following{/other_user}",
      "gists_url": "https://api.github.com/users/cstml/gists{/gist_id}",
      "starred_url": "https://api.github.com/users/cstml/starred{/owner}{/repo}",
      "subscriptions_url": "https://api.github.com/users/cstml/subscriptions",
      "organizations_url": "https://api.github.com/users/cstml/orgs",
      "repos_url": "https://api.github.com/users/cstml/repos",
      "events_url": "https://api.github.com/users/cstml/events{/privacy}",
      "received_events_url": "https://api.github.com/users/cstml/received_events",
      "type": "User",
      "site_admin": false
    },
    "labels": [
      {
        "id": 4778998986,
        "node_id": "LA_kwDOIZpwYM8AAAABHNm8yg",
        "url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/labels/audit",
        "name": "audit",
        "color": "175C86",
        "default": false,
        "description": ""
      },
      {
        "id": 4788493169,
        "node_id": "LA_kwDOIZpwYM8AAAABHWqbcQ",
        "url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/labels/audit-meta",
        "name": "audit-meta",
        "color": "A87138",
        "default": false,
        "description": ""
      }
    ],
    "state": "open",
    "locked": false,
    "assignee": null,
    "assignees": [

    ],
    "milestone": null,
    "comments": 0,
    "created_at": "2022-11-09T09:54:26Z",
    "updated_at": "2022-11-25T11:38:53Z",
    "closed_at": null,
    "author_association": "COLLABORATOR",
    "active_lock_reason": null,
    "body": "# Audit Scope \r\n\r\nThe Audit aims to achieve the following objectives:\r\n\r\n1. Review proposed fixes/comments to previous [Tweag Audit][tweag-report].\r\n  - [x] #5 \r\n  - [x] #2 \r\n  - [x] #3 \r\n  - [ ] #7 \r\n  - [ ] #8 \r\n  - [x] #9 \r\n  - [ ] #10 \r\n  - [x] #11 \r\n  - [x] #12 \r\n  - [ ] #13 \r\n  - [ ] #14 \r\n\r\n2. Review proposed fixes/comments to [findings since Tweag Audit](https://github.com/mlabs-haskell/optim-onchain-audit/issues/6). \r\n- [x] #16 \r\n- [x] #18 \r\n- [x] #19 \r\n- [x] #20 \r\n- [x] #25 \r\n- [x] #21 \r\n- [x] #22 \r\n- [x] #23 \r\n- [x] #24 \r\n\r\n4. Review contracts for any additional vulnerabilities against MLabs list of [Vulnerability types](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2).  \r\n- [ ] #15 \r\n- [ ] #27 \r\n- [ ] #28 \r\n\r\n5. Create an audit report capturing findings.\r\n- [ ] #17 \r\n\r\n# Documentation \r\n\r\n1. [Optim specification `eaff83fc5ce5544aa5c34d822509189fec336138`][optim-spec]\r\n1. [Optim implementation `6a16307904f25da1bf22fe1722b02596a6c6ab79`][optim-implementation]\r\n1. [Optim dAPP][optim-dapp]\r\n1. Inter-Audit Fixes document #6  \r\n3. [Optim Tweag Audit Report][tweag-report]\r\n\r\n# Methodology \r\n\r\n# Vulnerabilities \r\n- [ ] #32 \r\n- [ ] #31 \r\n- [ ] #30 \r\n\r\n<!-- Add Link to any found vulnerabilties -->\r\n\r\n<!-- Refs -->\r\n[optim-spec]: https://github.com/mlabs-haskell/optim-spec/commit/eaff83fc5ce5544aa5c34d822509189fec336138 \"Optim Specification\"\r\n[optim-implementation]: https://github.com/mlabs-haskell/optim-onchain/tree/audit-fixes \"Optim Implementation\"\r\n[optim-dapp]: https://preview.optim.finance/dashboard \"Optim DApp\"\r\n[tweag-report]: https://github.com/mlabs-haskell/optim-spec/blob/eaff83fc5ce5544aa5c34d822509189fec336138/audit/tweag-audit-report.pdf \"Tweag Audit Report\"",
    "reactions": {
      "url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1/reactions",
      "total_count": 0,
      "+1": 0,
      "-1": 0,
      "laugh": 0,
      "hooray": 0,
      "confused": 0,
      "heart": 0,
      "rocket": 0,
      "eyes": 0
    },
    "timeline_url": "https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues/1/timeline",
    "performed_via_github_app": null,
    "state_reason": null
  }
-}
