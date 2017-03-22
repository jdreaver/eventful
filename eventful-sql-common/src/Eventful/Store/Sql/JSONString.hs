module Eventful.Store.Sql.JSONString
  ( JSONString
  ) where

import Data.Aeson
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import Database.Persist
import Database.Persist.Sql

import Eventful.Serializable

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString { unJSONString :: Text }
  deriving (Eq, PersistField)

instance PersistFieldSql JSONString where
  sqlType _ = SqlOther "jsonb"

instance Show JSONString where
  show = show . unJSONString

instance (ToJSON a, FromJSON a) => Serializable a JSONString where
  serialize = encodeJSON
  deserialize = decodeJSON
  deserializeEither = decodeJSONEither

encodeJSON :: (ToJSON a) => a -> JSONString
encodeJSON = JSONString . TLE.decodeUtf8 . encode

decodeJSON :: (FromJSON a) => JSONString -> Maybe a
decodeJSON = decode . TLE.encodeUtf8 . unJSONString

decodeJSONEither :: (FromJSON a) => JSONString -> Either String a
decodeJSONEither = eitherDecode . TLE.encodeUtf8 . unJSONString
