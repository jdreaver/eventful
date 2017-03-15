module Eventful.Store.DynamoDB.DynamoJSON
  ( DynamoJSON (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Eventful.Serializable

-- | A more specific type than just ByteString for JSON data.
newtype DynamoJSON = DynamoJSON { unDynamoJSON :: Text }
  deriving (Eq)

instance Show DynamoJSON where
  show = show . unDynamoJSON

instance (ToJSON a, FromJSON a) => Serializable a DynamoJSON where
  serialize = encodeJSON
  deserialize = decodeJSON
  deserializeEither = decodeJSONEither

encodeJSON :: (ToJSON a) => a -> DynamoJSON
encodeJSON = DynamoJSON . TL.toStrict . TLE.decodeUtf8 . encode

decodeJSON :: (FromJSON a) => DynamoJSON -> Maybe a
decodeJSON = decode . TLE.encodeUtf8 . TL.fromStrict . unDynamoJSON

decodeJSONEither :: (FromJSON a) => DynamoJSON -> Either String a
decodeJSONEither = eitherDecode . TLE.encodeUtf8 . TL.fromStrict . unDynamoJSON
