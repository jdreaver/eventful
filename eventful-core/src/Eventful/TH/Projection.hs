{-# LANGUAGE QuasiQuotes #-}

module Eventful.TH.Projection
  ( mkProjection
  ) where

import Data.Char (toLower)
import Language.Haskell.TH

import Eventful.Projection
import Eventful.TH.SumType

-- | Creates a 'Projection' for a given type and a list of events. The user of
-- this function also needs to provide apply functions for each event. For
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
--    applyEventA :: MyState -> EventA -> MyState
--    applyEventA (MyState x) EventA = MyState (x + 1)
--
--    applyEventB :: MyState -> EventB -> MyState
--    applyEventB (MyState x) EventB = MyState (x - 1)
-- @
--
-- This will produce the following:
--
-- @
--    data MyStateEvent = MyStateEventA !EventA | MyStateEventB !EventB
--
--    applyMyStateEvent :: MyState -> MyStateEvent -> MyState
--    applyMyStateEvent state (MyStateEventA event) = applyEventA state event
--    applyMyStateEvent state (MyStateEventB event) = applyEventB state event
--
--    type MyStateProjection = Projection MyState MyStateEvent
--
--    myStateProjection :: MyStateProjection
--    myStateProjection = Projection myStateDefault applyMyStateEvent
-- @
mkProjection :: Name -> Name -> [Name] -> Q [Dec]
mkProjection stateName stateDefault events = do
  -- Make event sum type
  let eventTypeName = nameBase stateName ++ "Event"
  sumTypeDecls <- mkSumType eventTypeName (nameBase stateName ++) events

  -- Make function to apply events from sum type to handlers.
  let applyFuncName = mkName $ "apply" ++ eventTypeName
  applyFuncType <- [t| $(conT stateName) -> $(conT $ mkName eventTypeName) -> $(conT stateName) |]
  applyFuncBodies <- mapM (applyFuncBody stateName) events
  let
    applyTypeDecls =
      [ SigD applyFuncName applyFuncType
      , FunD applyFuncName applyFuncBodies
      ]

  -- Make the projection type
  projectionType <- [t| Projection $(conT stateName) $(conT $ mkName eventTypeName) |]
  let
    projectionTypeName = mkName $ nameBase stateName ++ "Projection"
    projectionTypeDecl = TySynD projectionTypeName [] projectionType

  -- Make the projection
  projectionFuncExpr <- [e| Projection $(varE stateDefault) $(varE applyFuncName) |]
  let
    projectionFuncName = mkName $ firstCharToLower (nameBase stateName) ++ "Projection"
    projectionFuncClause = Clause [] (NormalB projectionFuncExpr) []
    projectionDecls =
      [ SigD projectionFuncName (ConT projectionTypeName)
      , FunD projectionFuncName [projectionFuncClause]
      ]

  return $ sumTypeDecls ++ applyTypeDecls ++ [projectionTypeDecl] ++ projectionDecls

applyFuncBody :: Name -> Name -> Q Clause
applyFuncBody stateName event = do
  let
    statePattern = VarP (mkName "state")
    eventPattern = ConP (mkName $ nameBase stateName ++ nameBase event) [VarP (mkName "event")]
    applyFuncName = mkName $ "apply" ++ nameBase event
  constructor <- [e| $(varE applyFuncName) $(varE $ mkName "state") $(varE $ mkName "event") |]
  return $ Clause [statePattern, eventPattern] (NormalB constructor) []

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x:xs) = toLower x : xs
