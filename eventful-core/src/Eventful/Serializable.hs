module Eventful.Serializable
  ( Serializable (..)
  ) where

import Data.Aeson
import Data.Dynamic

-- | This type class is used to serialize and deserialize events in event
-- stores and event buses. We use a type class for this because we can ensure
-- the serialization method is unique for a given pair of types.
class Serializable a b where
  serialize :: a -> b
  deserialize :: b -> Maybe a

  -- | Deserialize with additional information on failure
  deserializeEither :: b -> Either String a
  deserializeEither = maybe (Left "Serializable: Failed to deserialize") Right . deserialize

instance (Typeable a) => Serializable a Dynamic where
  serialize = toDyn
  deserialize = fromDynamic

instance (ToJSON a, FromJSON a) => Serializable a Value where
  serialize = toJSON
  deserialize x =
    case fromJSON x of
      Success a -> Just a
      Error _ -> Nothing
  deserializeEither x =
    case fromJSON x of
      Success a -> Right a
      Error e -> Left e
