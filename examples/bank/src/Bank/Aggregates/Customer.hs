module Bank.Aggregates.Customer
  ( Customer (..)
  , customerProjection
  , customerAggregate
  ) where

import Data.Aeson.TH

import Eventful

import Bank.Commands
import Bank.Events
import Bank.Json

data Customer =
  Customer
  { customerName :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

applyCustomerEvent :: Customer -> BankEvent -> Customer
applyCustomerEvent customer (CustomerCreated' (CustomerCreated name)) = customer { customerName = Just name }
applyCustomerEvent customer _ = customer

customerProjection :: BankProjection Customer
customerProjection = Projection (Customer Nothing) applyCustomerEvent

applyCustomerCommand :: Customer -> BankCommand -> [BankEvent]
applyCustomerCommand customer (CreateCustomer' (CreateCustomer name)) =
  case customerName customer of
    Nothing -> [CustomerCreated' $ CustomerCreated name]
    Just _ -> [CustomerCreationRejected' $ CustomerCreationRejected "Customer already exists"]
applyCustomerCommand _ _ = []

customerAggregate :: BankAggregate Customer
customerAggregate = Aggregate applyCustomerCommand customerProjection
