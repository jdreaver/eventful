{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module BankCommon.Models
  ( BankEvent (..)
  , BankCommand (..)
  , accountEventSerializer
  , accountCommandSerializer
  , accountBankProjection
  , accountBankCommandHandler
  , customerEventSerializer
  , customerCommandSerializer
  , customerBankProjection
  , customerBankCommandHandler
  , module X
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful
import Eventful.TH

import BankCommon.Json
import BankCommon.Models.Account as X
import BankCommon.Models.Customer as X

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

accountBankCommandHandler :: CommandHandler Account BankEvent BankCommand
accountBankCommandHandler = serializedCommandHandler accountCommandHandler accountEventSerializer accountCommandSerializer

mkSumTypeSerializer "customerEventSerializer" ''CustomerEvent ''BankEvent
mkSumTypeSerializer "customerCommandSerializer" ''CustomerCommand ''BankCommand

customerBankProjection :: Projection Customer BankEvent
customerBankProjection = serializedProjection customerProjection customerEventSerializer

customerBankCommandHandler :: CommandHandler Customer BankEvent BankCommand
customerBankCommandHandler = serializedCommandHandler customerCommandHandler customerEventSerializer customerCommandSerializer
