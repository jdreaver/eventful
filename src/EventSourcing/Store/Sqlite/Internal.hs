{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventSourcing.Store.Sqlite.Internal
  ( JSONString
  ) where

import Data.Aeson
import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Database.Persist
import Database.Persist.Sql
import Data.UUID

import EventSourcing.Store.Class
import EventSourcing.UUID

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


instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
