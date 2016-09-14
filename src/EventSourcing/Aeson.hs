-- | Common Aeson helpers for event types.

module EventSourcing.Aeson
  ( module X
  , sumRecordOptions
  , unPrefix
  ) where

import Data.Aeson as X (FromJSON(..), ToJSON(..), encode, decode)
import Data.Aeson.TH as X
import Data.Char (toLower)
import Data.Monoid ((<>))

import Prelude

-- | Better default options for sum types of records.
sumRecordOptions :: Options
sumRecordOptions =
  defaultOptions
  { sumEncoding = TaggedObject "type" "contents"
  , allNullaryToStringTag = False
  }

-- | Aeson Options that remove the prefix from fields
unPrefix :: String -> Options
unPrefix prefix = sumRecordOptions
  { fieldLabelModifier = unCapitalize . dropPrefix prefix }

-- | Lower case leading character
unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

-- | Remove given prefix
dropPrefix :: String -> String -> String
dropPrefix prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " <> pre
    go [] (c:cs) = c : cs
    go (p:preRest) (c:cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " <>  (p:preRest)  <> " " <> (c:cRest)

    contextual msg = "dropPrefix: " <> msg <> ". " <> prefix <> " " <> input
