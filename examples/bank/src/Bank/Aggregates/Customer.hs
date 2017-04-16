module Bank.Aggregates.Customer
  ( Customer (..)
  , CustomerEvent (..)
  , customerEventSerializer
  , CustomerProjection
  , customerProjection
  , CustomerCommand (..)
  , CreateCustomerData (..)
  , CustomerCommandError (..)
  , CustomerAggregate
  , customerAggregate
  ) where

import Data.Aeson.TH

import Eventful

import Bank.Events
import Bank.Json

data Customer =
  Customer
  { customerName :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

mkEventSumType' "CustomerEvent"
  [ ''CustomerCreated
  ]
deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

mkSumTypeSerializer "customerEventSerializer" ''CustomerEvent ''BankEvent

applyCustomerEvent :: Customer -> CustomerEvent -> Customer
applyCustomerEvent customer (CustomerCreated' (CustomerCreated name)) =
  customer { customerName = Just name }

type CustomerProjection = Projection Customer CustomerEvent

customerProjection :: CustomerProjection
customerProjection = Projection (Customer Nothing) applyCustomerEvent

data CustomerCommand
  = CreateCustomer CreateCustomerData
  deriving (Show, Eq)

data CreateCustomerData =
  CreateCustomerData
  { createCustomerDataName :: String
  } deriving (Show, Eq)

data CustomerCommandError
  = CustomerAlreadyExistsError

deriveJSON defaultOptions ''CustomerCommandError

applyCustomerCommand :: Customer -> CustomerCommand -> Either CustomerCommandError [CustomerEvent]
applyCustomerCommand customer (CreateCustomer (CreateCustomerData name)) =
  case customerName customer of
    Nothing -> Right [CustomerCreated' $ CustomerCreated name]
    Just _ -> Left CustomerAlreadyExistsError

type CustomerAggregate = Aggregate Customer CustomerEvent CustomerCommand CustomerCommandError

customerAggregate :: CustomerAggregate
customerAggregate = Aggregate applyCustomerCommand customerProjection
