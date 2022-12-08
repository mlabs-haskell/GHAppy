{-# OPTIONS_GHC -Wwarn #-}

module GHAppy.YamlInterface where

import Control.Applicative
import Data.Yaml
import GHC.Generics

data YamlInterface = YamlInterface
  { composer :: [ComposerInstruction]
  , gHAppy :: [GHAppyInstruction]
  }
  deriving (Eq, Show, Generic, ToJSON)

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
  deriving (Eq, Show, Generic, ToJSON)

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
  deriving (Eq, Show, Generic, ToJSON)

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
  = HasLabel String
  | IsOpen Bool
  deriving (Eq, Show, Generic)

instance FromJSON Filter where
  parseJSON (Object o) =
    (HasLabel <$> o .: "hasLabel")
      <|> (IsOpen <$> o .: "isOpen")
  parseJSON _ = mempty

instance ToJSON Filter where
  toJSON = \case
    HasLabel s -> object ["hasLabel" .= s]
    IsOpen b -> object ["isOpen" .= b]

main :: IO ()
main = do
  print =<< decodeFileEither @YamlInterface "./example/report2.yaml"
  pure ()
