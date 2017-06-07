{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventful.TH.Projection
  ( mkProjection
  ) where

import Data.Char (toLower)
import Language.Haskell.TH
import SumTypes.TH

import Eventful.Projection

-- | Creates a 'Projection' for a given type and a list of events. The user of
-- this function also needs to provide event handlers for each event. For
-- example:
--
-- @
--    data EventA = EventA
--    data EventB = EventB
--
--    data MyState = MyState Int
--
--    myStateDefault :: MyState
--    myStateDefault = MyState 0
--
--    mkProjection ''MyState 'myStateDefault [''EventA, ''EventB]
--
--    handleEventA :: MyState -> EventA -> MyState
--    handleEventA (MyState x) EventA = MyState (x + 1)
--
--    handleEventB :: MyState -> EventB -> MyState
--    handleEventB (MyState x) EventB = MyState (x - 1)
-- @
--
-- This will produce the following:
--
-- @
--    data MyStateEvent = MyStateEventA !EventA | MyStateEventB !EventB
--
--    handleMyStateEvent :: MyState -> MyStateEvent -> MyState
--    handleMyStateEvent state (MyStateEventA event) = handleEventA state event
--    handleMyStateEvent state (MyStateEventB event) = handleEventB state event
--
--    type MyStateProjection = Projection MyState MyStateEvent
--
--    myStateProjection :: MyStateProjection
--    myStateProjection = Projection myStateDefault handleMyStateEvent
-- @
mkProjection :: Name -> Name -> [Name] -> Q [Dec]
mkProjection stateName stateDefault events = do
  -- Make event sum type
  let eventTypeName = nameBase stateName ++ "Event"
  sumTypeDecls <- constructSumType eventTypeName defaultSumTypeOptions events

  -- Make function to handle events from sum type to handlers.
  let handleFuncName = mkName $ "handle" ++ eventTypeName
  handleFuncType <- [t| $(conT stateName) -> $(conT $ mkName eventTypeName) -> $(conT stateName) |]
  handleFuncBodies <- mapM (handleFuncBody stateName) events
  let
    handleTypeDecls =
      [ SigD handleFuncName handleFuncType
      , FunD handleFuncName handleFuncBodies
      ]

  -- Make the projection type
  projectionType <- [t| Projection $(conT stateName) $(conT $ mkName eventTypeName) |]
  let
    projectionTypeName = mkName $ nameBase stateName ++ "Projection"
    projectionTypeDecl = TySynD projectionTypeName [] projectionType

  -- Make the projection
  projectionFuncExpr <- [e| Projection $(varE stateDefault) $(varE handleFuncName) |]
  let
    projectionFuncName = mkName $ firstCharToLower (nameBase stateName) ++ "Projection"
    projectionFuncClause = Clause [] (NormalB projectionFuncExpr) []
    projectionDecls =
      [ SigD projectionFuncName (ConT projectionTypeName)
      , FunD projectionFuncName [projectionFuncClause]
      ]

  return $ sumTypeDecls ++ handleTypeDecls ++ [projectionTypeDecl] ++ projectionDecls

handleFuncBody :: Name -> Name -> Q Clause
handleFuncBody stateName event = do
  let
    statePattern = VarP (mkName "state")
    eventPattern = ConP (mkName $ nameBase stateName ++ nameBase event) [VarP (mkName "event")]
    handleFuncName = mkName $ "handle" ++ nameBase event
  constructor <- [e| $(varE handleFuncName) $(varE $ mkName "state") $(varE $ mkName "event") |]
  return $ Clause [statePattern, eventPattern] (NormalB constructor) []

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x:xs) = toLower x : xs
