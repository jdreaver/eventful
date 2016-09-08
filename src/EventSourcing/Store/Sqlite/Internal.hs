module EventSourcing.Store.Sqlite.Internal
  ( JSONString
  ) where

import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Database.Persist
import Database.Persist.Sql

import EventSourcing.Store.Class
import TodoCommon

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString { unJSONString :: ByteString }
  deriving (Eq, PersistField, PersistFieldSql)

instance Show JSONString where
  show = show . unJSONString

instance (ToJSON a, FromJSON a) => Serializable a JSONString where
  serialize = encodeJSON
  deserialize = decodeJSON

encodeJSON :: (ToJSON a) => a -> JSONString
encodeJSON = JSONString . toStrict . encode

decodeJSON :: (FromJSON a) => JSONString -> Maybe a
decodeJSON = decode . fromStrict . unJSONString
