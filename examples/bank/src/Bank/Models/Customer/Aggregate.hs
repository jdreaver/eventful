{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Aggregate
  ( CustomerCommand (..)
  , customerAggregate
  ) where


import Eventful
import Eventful.TH

import Bank.Models.Customer.Commands
import Bank.Models.Customer.Events
import Bank.Models.Customer.Projection

mkSumType "CustomerCommand" (++ "CustomerCommand") customerCommands

handleCustomerCommand :: Customer -> CustomerCommand -> [CustomerEvent]
handleCustomerCommand customer (CreateCustomerCustomerCommand (CreateCustomer name)) =
  case customerName customer of
    Nothing -> [CustomerCreatedCustomerEvent $ CustomerCreated name]
    Just _ -> [CustomerCreationRejectedCustomerEvent $ CustomerCreationRejected "Customer already exists"]

customerAggregate :: Aggregate Customer CustomerEvent CustomerCommand
customerAggregate = Aggregate handleCustomerCommand customerProjection
