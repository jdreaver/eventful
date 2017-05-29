{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Projection
  ( Customer (..)
  , CustomerEvent (..)
  , customerProjection
  ) where

import Data.Aeson.TH

import Eventful
import Eventful.TH

import Bank.Models.Customer.Events
import Bank.Json

data Customer =
  Customer
  { customerName :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

mkSumType "CustomerEvent" (++ "CustomerEvent") customerEvents

deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent customer (CustomerCreatedCustomerEvent (CustomerCreated name)) = customer { customerName = Just name }
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing) handleCustomerEvent
