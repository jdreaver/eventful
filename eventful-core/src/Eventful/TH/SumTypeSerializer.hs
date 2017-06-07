{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventful.TH.SumTypeSerializer
  ( mkSumTypeSerializer
  ) where

import Data.Char (toLower)
import Language.Haskell.TH
import SumTypes.TH

-- | This is a template haskell function that creates a 'Serializer' between
-- two sum types. The first sum type must be a subset of the second sum type.
-- This is useful in situations where you define all the events in your system
-- in one type, and you want to create sum types that are subsets for each
-- 'Projection'.
--
-- For example, assume we have the following three event types and two sum
-- types holding these events:
--
-- @
--    data EventA = EventA
--    data EventB = EventB
--    data EventC = EventC
--
--    data AllEvents
--      = AllEventsEventA EventA
--      | AllEventsEventB EventB
--      | AllEventsEventC EventC
--
--    data MyEvents
--      = MyEventsEventA EventA
--      | MyEventsEventB EventB
-- @
--
-- In this case, @AllEvents@ holds all the events in our system, and @MyEvents@
-- holds some subset of @AllEvents@. If we run
--
-- @
--    mkSumTypeSerializer "myEventsSerializer" ''MyEvents ''AllEvents
-- @
--
-- we will produce the following code:
--
-- @
--    -- Serialization function
--    myEventsToAllEvents :: MyEvents -> AllEvents
--    myEventsToAllEvents (MyEventsEventA e) = AllEventsEventA e
--    myEventsToAllEvents (MyEventsEventB e) = AllEventsEventB e
--
--    -- Deserialization function
--    allEventsToMyEvents :: AllEvents -> Maybe MyEvents
--    allEventsToMyEvents (AllEventsEventA e) = Just (MyEventsEventA e)
--    allEventsToMyEvents (AllEventsEventB e) = Just (MyEventsEventB e)
--    allEventsToMyEvents _ = Nothing
--
--    -- Serializer
--    myEventsSerializer :: Serializer MyEvents AllEvents
--    myEventsSerializer = simpleSerializer myEventsToAllEvents allEventsToMyEvents
-- @
mkSumTypeSerializer :: String -> Name -> Name -> Q [Dec]
mkSumTypeSerializer serializerName sourceType targetType = do
  -- Construct the serialization function
  let
    serializeFuncName = firstCharToLower (nameBase sourceType) ++ "To" ++ nameBase targetType
    deserializeFuncName = firstCharToLower (nameBase targetType) ++ "To" ++ nameBase sourceType
  serializeDecls <- sumTypeConverter serializeFuncName sourceType targetType
  deserializeDecls <- partialSumTypeConverter deserializeFuncName targetType sourceType

  -- Construct the serializer
  serializerTypeDecl <- [t| $(conT $ mkName "Serializer") $(conT sourceType) $(conT targetType) |]
  serializerExp <- [e| $(varE $ mkName "simpleSerializer") $(varE $ mkName serializeFuncName) $(varE $ mkName deserializeFuncName) |]
  let
    serializerClause = Clause [] (NormalB serializerExp) []

  return $
    [ SigD (mkName serializerName) serializerTypeDecl
    , FunD (mkName serializerName) [serializerClause]
    ] ++ serializeDecls ++ deserializeDecls

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x:xs) = toLower x : xs
