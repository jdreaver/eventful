{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models
  ( BankEvent (..)
  , BankCommand (..)
  , accountEventSerializer
  , accountCommandSerializer
  , accountBankProjection
  , accountBankAggregate
  , customerEventSerializer
  , customerCommandSerializer
  , customerBankProjection
  , customerBankAggregate
  , module X
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful
import Eventful.TH

import Bank.Json
import Bank.Models.Account as X
import Bank.Models.Customer as X

constructSumType "BankEvent" (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Event") }) $
  concat
  [ accountEvents
  , customerEvents
  ]

deriving instance Show BankEvent
deriving instance Eq BankEvent

deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "Event" }) ''BankEvent

constructSumType "BankCommand" (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Command") }) $
  concat
  [ accountCommands
  , customerCommands
  ]

deriving instance Show BankCommand
deriving instance Eq BankCommand

mkSumTypeSerializer "accountEventSerializer" ''AccountEvent ''BankEvent
mkSumTypeSerializer "accountCommandSerializer" ''AccountCommand ''BankCommand

accountBankProjection :: Projection Account BankEvent
accountBankProjection = serializedProjection accountProjection accountEventSerializer

accountBankAggregate :: Aggregate Account BankEvent BankCommand
accountBankAggregate = serializedAggregate accountAggregate accountEventSerializer accountCommandSerializer

mkSumTypeSerializer "customerEventSerializer" ''CustomerEvent ''BankEvent
mkSumTypeSerializer "customerCommandSerializer" ''CustomerCommand ''BankCommand

customerBankProjection :: Projection Customer BankEvent
customerBankProjection = serializedProjection customerProjection customerEventSerializer

customerBankAggregate :: Aggregate Customer BankEvent BankCommand
customerBankAggregate = serializedAggregate customerAggregate customerEventSerializer customerCommandSerializer
