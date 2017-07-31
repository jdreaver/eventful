{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Eventful.Serializer
  ( -- * Class
    Serializer (..)
  , simpleSerializer
  , composeSerializers
    -- * Common serializers
  , idSerializer
  , traverseSerializer
  , jsonSerializer
  , jsonTextSerializer
  , dynamicSerializer
    -- * Sum types
  , EventSumType (..)
  , eventSumTypeSerializer
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Dynamic
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable (typeOf)
import GHC.Generics

-- | A 'Serializer' describes the injective conversion between types @a@ and
-- @b@. In plain English, this means that you can go from @a@ to @b@, and you
-- can 'Maybe' go from @b@ back to @a@. This is often used to serialize events
-- to an event store, and then deserialize them back.
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

-- | Apply an intermediate 'Serializer' to a serializer to go from type @a@ to
-- @c@ with @b@ in the middle. Note that with deserializing, if the conversion
-- from @c@ to @b@ or from @b@ to @a@ fails, the whole deserialization fails.
composeSerializers :: Serializer a b -> Serializer b c -> Serializer a c
composeSerializers serializer1 serializer2 = Serializer serialize' deserialize' deserializeEither'
  where
    serialize' = serialize serializer2 . serialize serializer1
    deserialize' x = deserialize serializer2 x >>= deserialize serializer1
    deserializeEither' x = deserializeEither serializer2 x >>= deserializeEither serializer1

-- | Simple "serializer" using 'id'. Useful for when an API requires a
-- serializer but you don't need to actually change types.
idSerializer :: Serializer a a
idSerializer = simpleSerializer id Just

-- | Uses 'Traversable' to wrap a 'Serializer'.
traverseSerializer
  :: (Traversable t)
  => Serializer a b
  -> Serializer (t a) (t b)
traverseSerializer Serializer{..} =
  Serializer serialize' deserialize' deserializeEither'
  where
    serialize' = fmap serialize
    deserialize' = traverse deserialize
    deserializeEither' = traverse deserializeEither

-- | A 'Serializer' for aeson 'Value's.
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
-- stores that store JSON values as text.
jsonTextSerializer :: (ToJSON a, FromJSON a) => Serializer a TL.Text
jsonTextSerializer =
  Serializer
  { serialize = TLE.decodeUtf8 . encode
  , deserialize = decode . TLE.encodeUtf8
  , deserializeEither = eitherDecode . TLE.encodeUtf8
  }

-- | A 'Serializer' for 'Dynamic' values using 'toDyn' and 'fromDynamic'.
dynamicSerializer :: (Typeable a) => Serializer a Dynamic
dynamicSerializer = simpleSerializer toDyn fromDynamic

-- | A 'Serializer' from one 'EventSumType' instance to another. WARNING: If
-- not all events in the source 'EventSumType' are in the @serialized@
-- 'EventSumType', then this function will be partial!
eventSumTypeSerializer :: (Typeable a, EventSumType a, EventSumType b) => Serializer a b
eventSumTypeSerializer = simpleSerializer serialize' deserialize'
  where
    serialize' event =
      fromMaybe
      (error $ "Failure in eventSumTypeSerializer. Can't serialize " ++ show (typeOf event))
      (eventFromDyn $ eventToDyn event)
    deserialize' = eventFromDyn . eventToDyn

-- | This is a type class for serializing sum types of events to 'Dynamic'
-- without the associated constructor. This is useful when transforming between
-- two sum types of events. A common pattern is to put all the events in an
-- application in one big event sum type, and then have a smaller sum type for
-- each 'Projection'. Then, you can use 'eventSumTypeSerializer' to transform
-- between the two.
--
-- It is meant to be derived with 'Generic'. For example:
--
-- @
--    data EventA = EventA deriving (Show)
--    data EventB = EventB deriving (Show)
--    data EventC = EventC deriving (Show)
--
--    data AllEvents
--      = AllEventsEventA EventA
--      | AllEventsEventB EventB
--      | AllEventsEventC EventC
--      deriving (Show, Generic)
--
--    instance EventSumType AllEvents
--
--    data MyEvents
--      = MyEventsEventA EventA
--      | MyEventsEventB EventB
--      deriving (Show, Generic)
--
--    instance EventSumType MyEvents
-- @
--
-- Now we can serialize to 'Dynamic' without a constructor tag:
--
-- >>> eventToDyn (MyEventsEventA EventA)
-- <<EventA>>
--
-- We can also go from a 'MyEvents' value to an 'AllEvents' value:
--
-- >>> eventFromDyn (eventToDyn (MyEventsEventA EventA)) :: Maybe AllEvents
-- Just (AllEventsEventA EventA)
--
class EventSumType a where
  -- | Convert an event to a 'Dynamic' without the constructor tag
  eventToDyn :: a -> Dynamic

  -- | Go from a 'Dynamic' to an event with the constructor tag. Note, this
  -- function is @O(n)@ to the number of constructors.
  eventFromDyn :: Dynamic -> Maybe a

  default eventToDyn :: (Generic a, EventSumType' (Rep a)) => a -> Dynamic
  eventToDyn x = eventToDyn' (from x)

  default eventFromDyn :: (Generic a, EventSumType' (Rep a)) => Dynamic -> Maybe a
  eventFromDyn = fmap to . eventFromDyn'

-- Auxiliary type class for 'EventSumType' Generic fun
class EventSumType' f where
  eventToDyn' :: f p -> Dynamic
  eventFromDyn' :: Dynamic -> Maybe (f p)

-- M1 is the top-level metadata. We don't need the metadata so we just pass on
-- through.
instance (EventSumType' f) => EventSumType' (M1 i t f) where
  eventToDyn' (M1 x) = eventToDyn' x
  eventFromDyn' = fmap M1 . eventFromDyn'

-- The :+: operator is for when a type has multiple constructors. When
-- serializing, we just pass on through. When deserializing, we try the first
-- constructor, and if that fails then the second.
instance (EventSumType' f, EventSumType' g) => EventSumType' (f :+: g) where
  eventToDyn' (L1 x) = eventToDyn' x
  eventToDyn' (R1 x) = eventToDyn' x
  eventFromDyn' dyn = (L1 <$> eventFromDyn' dyn) <|> (R1 <$> eventFromDyn' dyn)

-- K1 R represents an actual constructor. This is where we do the actual
-- conversion to/from 'Dynamic'.
instance (Typeable c) => EventSumType' (K1 R c) where
  eventToDyn' (K1 x) = toDyn x
  eventFromDyn' dyn = K1 <$> fromDynamic dyn
