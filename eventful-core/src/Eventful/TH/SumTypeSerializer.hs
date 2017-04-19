{-# LANGUAGE QuasiQuotes #-}

module Eventful.TH.SumTypeSerializer
  ( mkSumTypeSerializer
  ) where

import Data.Char (toLower)
import Data.List (lookup)
import Language.Haskell.TH

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
  -- Get the constructors for both types and match them up based on event type.
  sourceConstructors <- typeConstructors sourceType
  targetConstructors <- typeConstructors targetType
  bothConstructors <- mapM (matchConstructor targetConstructors) sourceConstructors

  -- Construct the serialization function
  let
    serializeFuncName = mkName $ firstCharToLower (nameBase sourceType) ++ "To" ++ nameBase targetType
    serializeFuncClauses = map mkSerializeFunc bothConstructors
  serializeTypeDecl <- [t| $(conT sourceType) -> $(conT targetType) |]

  -- Construct the deserialization function
  let
    deserializeFuncName = mkName $ firstCharToLower (nameBase targetType) ++ "To" ++ nameBase sourceType
    wildcardDeserializeClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
    deserializeFuncClauses = map mkDeserializeFunc bothConstructors ++ [wildcardDeserializeClause]
  deserializeTypeDecl <- [t| $(conT targetType) -> Maybe $(conT sourceType) |]

  -- Construct the serializer
  serializerTypeDecl <- [t| $(conT $ mkName "Serializer") $(conT sourceType) $(conT targetType) |]
  serializerExp <- [e| $(varE $ mkName "simpleSerializer") $(varE serializeFuncName) $(varE deserializeFuncName) |]
  let
    serializerClause = Clause [] (NormalB serializerExp) []

  return
    [ -- Serialization
      SigD serializeFuncName serializeTypeDecl
    , FunD serializeFuncName serializeFuncClauses

      -- Deserialization
    , SigD deserializeFuncName deserializeTypeDecl
    , FunD deserializeFuncName deserializeFuncClauses

      -- Serializer
    , SigD (mkName serializerName) serializerTypeDecl
    , FunD (mkName serializerName) [serializerClause]
    ]

-- | Extract the constructors and event types for the given type.
typeConstructors :: Name -> Q [(Type, Name)]
typeConstructors typeName = do
  info <- reify typeName
  case info of
    (TyConI (DataD _ _ _ _ constructors _)) -> mapM go constructors
      where
        go (NormalC name []) = fail $ "Constructor " ++ nameBase name ++ " doesn't have any arguments"
        go (NormalC name [(_, type')]) = return (type', name)
        go (NormalC name _) = fail $ "Constructor " ++ nameBase name ++ " has more than one argument"
        go _ = fail $ "Invalid constructor in " ++ nameBase typeName
    _ -> fail $ nameBase typeName ++ " must be a sum type"

-- | Find the corresponding target constructor for a given source constructor.
matchConstructor :: [(Type, Name)] -> (Type, Name) -> Q BothConstructors
matchConstructor targetConstructors (type', sourceConstructor) = do
  targetConstructor <-
    maybe
    (fail $ "Can't find constructor in target type corresponding to " ++ nameBase sourceConstructor)
    return
    (lookup type' targetConstructors)
  return $ BothConstructors type' sourceConstructor targetConstructor

-- | Utility type to hold the source and target constructors for a given event
-- type.
data BothConstructors =
  BothConstructors
  { eventType :: Type
  , sourceConstructor :: Name
  , targetConstructor :: Name
  }

-- | Construct the TH function 'Clause' for the serialization function for a
-- given type.
mkSerializeFunc :: BothConstructors -> Clause
mkSerializeFunc BothConstructors{..} =
  let
    patternMatch = ConP sourceConstructor [VarP (mkName "e")]
    constructor = AppE (ConE targetConstructor) (VarE (mkName "e"))
  in Clause [patternMatch] (NormalB constructor) []

-- | Construct the TH function 'Clause' for the deserialization function for a
-- given type.
mkDeserializeFunc :: BothConstructors -> Clause
mkDeserializeFunc BothConstructors{..} =
  let
    patternMatch = ConP targetConstructor [VarP (mkName "e")]
    constructor = AppE (ConE 'Just) (AppE (ConE sourceConstructor) (VarE (mkName "e")))
  in Clause [patternMatch] (NormalB constructor) []

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x:xs) = toLower x : xs
