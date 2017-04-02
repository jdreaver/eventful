module Eventful.Serializer
  ( Serializer (..)
  , simpleSerializer
  , jsonSerializer
  , jsonTextSerializer
  , dynamicSerializer
  ) where

import Data.Aeson
import Data.Dynamic
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- | Used to define how to serialize and deserialize events in event stores.
data Serializer a b =
  Serializer
  { serialize :: a -> b
  , deserialize :: b -> Maybe a
  , deserializeEither :: b -> Either String a
    -- ^ Deserialize with additional information on failure
  }

-- | Simple constructor to just use 'deserialize' to construct
-- 'deserializeEither'.
simpleSerializer
  :: (a -> b)
  -> (b -> Maybe a)
  -> Serializer a b
simpleSerializer serialize' deserialize' =
  Serializer
  { serialize = serialize'
  , deserialize = deserialize'
  , deserializeEither = maybe (Left "Serializable: Failed to deserialize") Right . deserialize'
  }

-- | A 'Serializer' for aeson 'Value's
jsonSerializer :: (ToJSON a, FromJSON a) => Serializer a Value
jsonSerializer =
  Serializer
  { serialize = toJSON
  , deserialize = \x ->
      case fromJSON x of
        Success a -> Just a
        Error _ -> Nothing
  , deserializeEither = \x ->
      case fromJSON x of
        Success a -> Right a
        Error e -> Left e
  }

-- | A 'Serializer' to convert JSON to/from lazy text. Useful for Sql event
-- stores.
jsonTextSerializer :: (ToJSON a, FromJSON a) => Serializer a TL.Text
jsonTextSerializer =
  Serializer
  { serialize = TLE.decodeUtf8 . encode
  , deserialize = decode . TLE.encodeUtf8
  , deserializeEither = eitherDecode . TLE.encodeUtf8
  }

-- | A 'Serializer' for 'Dynamic' values
dynamicSerializer :: (Typeable a) => Serializer a Dynamic
dynamicSerializer = simpleSerializer toDyn fromDynamic
