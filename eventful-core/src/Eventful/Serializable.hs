module Eventful.Serializable
  ( Serializable (..)
  ) where

import Data.Dynamic

-- | This type class is used to serialize and deserialize events in event
-- stores and event buses. We use a type class for this because we can ensure
-- the serialization method is unique for a given pair of types.
class Serializable a b where
  serialize :: a -> b
  deserialize :: b -> Maybe a

instance (Typeable a) => Serializable a Dynamic where
  serialize = toDyn
  deserialize = fromDynamic
