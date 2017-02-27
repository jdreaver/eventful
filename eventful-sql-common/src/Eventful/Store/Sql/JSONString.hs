{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventful.Store.Sql.JSONString
  ( JSONString
  ) where

import Data.Aeson
import Data.Proxy
import Data.Text.Lazy (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.UUID
import Database.Persist
import Database.Persist.Sql

import Eventful.Serializable
import Eventful.Store.Class
import Eventful.UUID

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString { unJSONString :: Text }
  deriving (Eq, PersistField)

instance PersistFieldSql JSONString where
  sqlType _ = SqlOther "json"

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

instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistDbSpecific t) =
    case uuidFromText (TE.decodeUtf8 t) of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PersistField EventVersion where
  toPersistValue = toPersistValue . unEventVersion
  fromPersistValue = fmap EventVersion . fromPersistValue

instance PersistFieldSql EventVersion where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField SequenceNumber where
  toPersistValue = toPersistValue . unSequenceNumber
  fromPersistValue = fmap SequenceNumber . fromPersistValue

instance PersistFieldSql SequenceNumber where
  sqlType _ = sqlType (Proxy :: Proxy Int)
