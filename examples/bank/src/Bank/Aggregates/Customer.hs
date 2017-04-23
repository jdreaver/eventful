module Bank.Aggregates.Customer
  ( Customer (..)
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

applyCustomerEvent :: Customer -> BankEvent -> Customer
applyCustomerEvent customer (CustomerCreatedEvent (CustomerCreated name)) = customer { customerName = Just name }
applyCustomerEvent customer _ = customer

type CustomerProjection = Projection Customer BankEvent

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

applyCustomerCommand :: Customer -> CustomerCommand -> Either CustomerCommandError [BankEvent]
applyCustomerCommand customer (CreateCustomer (CreateCustomerData name)) =
  case customerName customer of
    Nothing -> Right [CustomerCreatedEvent $ CustomerCreated name]
    Just _ -> Left CustomerAlreadyExistsError

type CustomerAggregate = Aggregate Customer BankEvent CustomerCommand CustomerCommandError

customerAggregate :: CustomerAggregate
customerAggregate = Aggregate applyCustomerCommand customerProjection
