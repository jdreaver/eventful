module Bank.Commands
  ( BankCommand (..)
  , BankCommandError (..)
  , UnknownCommand (..)
  , BankAggregate
  , module X
  ) where

import Data.Aeson.TH

import Eventful
import Eventful.TH

import Bank.Commands.Account as X
import Bank.Commands.Customer as X
import Bank.Events
import Bank.Json

mkSumType "BankCommand" (++ "'")
  [ --Account
    ''OpenAccount
  , ''CreditAccount
  , ''DebitAccount
  , ''TransferToAccount
  , ''AcceptTransfer

    -- Customer
  , ''CreateCustomer
  ]

deriving instance Show BankCommand
deriving instance Eq BankCommand

deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "'" }) ''BankCommand

data UnknownCommand = UnknownCommand
  deriving (Show, Eq)

mkSumType "BankCommandError" (++ "'")
  [ ''AccountCommandError
  , ''CustomerCommandError
  , ''UnknownCommand
  ]

deriving instance Show BankCommandError
deriving instance Eq BankCommandError

deriveJSON defaultOptions ''UnknownCommand
deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "'" }) ''BankCommandError

type BankAggregate state = Aggregate state BankEvent BankCommand BankCommandError
