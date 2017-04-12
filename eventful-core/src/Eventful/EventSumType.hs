module Eventful.EventSumType
  ( mkEventSumType
  , mkEventSumType'
  ) where

import Language.Haskell.TH

-- | This is a template haskell function that creates a sum type from a list of
-- events. This is very useful when creating an event type for a 'Projection'
-- from a list of events in your system. Here is an example:
--
-- @
--    data EventA = EventA
--    data EventB = EventB
--    data EventC = EventC
--
--    mkEventSumType "MyEvent" ("MyEvent" ++) [''EventA, ''EventB, ''EventC]
-- @
--
-- This will produce the following sum type:
--
-- @
--    data MyEvent
--      = MyEventEventA EventA
--      | MyEventEventB EventB
--      | MyEventEventC EventC
-- @
--
-- Note that you can use standalone deriving to derive any instances you want:
--
-- @
--    deriving instance Show MyEvent
--    deriving instance Eq MyEvent
-- @
mkEventSumType :: String -> (String -> String) -> [Name] -> Q [Dec]
mkEventSumType typeName mkConstructorName eventTypes = do
  let
    mkConstructor eventName =
      NormalC
      (mkName . mkConstructorName . nameBase $ eventName)
      [(Bang NoSourceUnpackedness SourceStrict, ConT eventName)]
    constructors = map mkConstructor eventTypes
  return [DataD [] (mkName typeName) [] Nothing constructors []]

-- | Variant of mkEventSumType that just appends a @'@ to each constructor.
--
-- @
--    mkEventSumType' name events = mkEventSumType name (++ "'") events
-- @
mkEventSumType' :: String -> [Name] -> Q [Dec]
mkEventSumType' name = mkEventSumType name (++ "'")
