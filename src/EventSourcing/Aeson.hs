-- | Common Aeson helpers for event types.

module EventSourcing.Aeson
  ( module X
  , sumRecordOptions
  , unPrefix
  , JSONString
  , encodeJSON
  , decodeJSON
  ) where

import Data.Aeson as X (FromJSON(..), ToJSON(..), encode, decode)
import Data.Aeson.TH as X
import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Sqlite

import Prelude

-- | Better default options
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

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString { unJSONString :: ByteString }
  deriving (Eq, PersistField, PersistFieldSql)

instance Show JSONString where
  show = show . unJSONString

encodeJSON :: (ToJSON a) => a -> JSONString
encodeJSON = JSONString . toStrict . encode

decodeJSON :: (FromJSON a) => JSONString -> Maybe a
decodeJSON = decode . fromStrict . unJSONString
