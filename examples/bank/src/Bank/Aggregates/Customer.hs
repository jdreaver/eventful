{-# LANGUAGE TemplateHaskell #-}

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

handleCustomerEvent :: Customer -> BankEvent -> Customer
handleCustomerEvent customer (CustomerCreatedEvent (CustomerCreated name)) = customer { customerName = Just name }
handleCustomerEvent customer _ = customer

customerProjection :: BankProjection Customer
customerProjection = Projection (Customer Nothing) handleCustomerEvent

handleCustomerCommand :: Customer -> BankCommand -> [BankEvent]
handleCustomerCommand customer (CreateCustomerCommand (CreateCustomer name)) =
  case customerName customer of
    Nothing -> [CustomerCreatedEvent $ CustomerCreated name]
    Just _ -> [CustomerCreationRejectedEvent $ CustomerCreationRejected "Customer already exists"]
handleCustomerCommand _ _ = []

customerAggregate :: BankAggregate Customer
customerAggregate = Aggregate handleCustomerCommand customerProjection
