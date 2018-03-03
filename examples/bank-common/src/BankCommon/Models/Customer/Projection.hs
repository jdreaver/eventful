{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module BankCommon.Models.Customer.Projection
  ( Customer (..)
  , CustomerEvent (..)
  , customerProjection
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful

import BankCommon.Models.Customer.Events
import BankCommon.Json

data Customer =
  Customer
  { customerName :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

constructSumType "CustomerEvent" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) customerEvents

deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent customer (CustomerCreatedCustomerEvent (CustomerCreated name)) = customer { customerName = Just name }
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing) handleCustomerEvent
