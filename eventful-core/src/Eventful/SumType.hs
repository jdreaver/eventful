module Eventful.SumType
  ( mkSumType
  , mkSumType'
  ) where

import Language.Haskell.TH

-- | This is a template haskell function that creates a sum type from a list of
-- types. This is very useful when creating an event type for a 'Projection'
-- from a list of events in your system. Here is an example:
--
-- @
--    data EventA = EventA
--    data EventB = EventB
--    data EventC = EventC
--
--    mkSumType "MyEvent" ("MyEvent" ++) [''EventA, ''EventB, ''EventC]
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
mkSumType :: String -> (String -> String) -> [Name] -> Q [Dec]
mkSumType typeName mkConstructorName eventTypes = do
  let
    mkConstructor eventName =
      NormalC
      (mkName . mkConstructorName . nameBase $ eventName)
      [(Bang NoSourceUnpackedness SourceStrict, ConT eventName)]
    constructors = map mkConstructor eventTypes
  return [DataD [] (mkName typeName) [] Nothing constructors []]

-- | Variant of mkSumType that just appends a @'@ to each constructor.
--
-- @
--    mkSumType' name events = mkSumType name (++ "'") events
-- @
mkSumType' :: String -> [Name] -> Q [Dec]
mkSumType' name = mkSumType name (++ "'")
