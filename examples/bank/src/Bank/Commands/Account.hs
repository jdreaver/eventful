module Bank.Commands.Account
  ( OpenAccount (..)
  , CreditAccount (..)
  , DebitAccount (..)
  , TransferToAccount (..)
  , AcceptTransfer (..)

  , AccountCommandError (..)
  , NotEnoughFundsData (..)
  ) where

import Data.Aeson.TH

import Eventful.UUID

import Bank.Json

data OpenAccount =
  OpenAccount
  { openAccountOwner :: UUID
  , openAccountInitialFunding :: Double
  } deriving (Show, Eq)

data CreditAccount =
  CreditAccount
  { creditAccountAmount :: Double
  , creditAccountReason :: String
  } deriving (Show, Eq)

data DebitAccount =
  DebitAccount
  { debitAccountAmount :: Double
  , debitAccountReason :: String
  } deriving (Show, Eq)

data TransferToAccount =
  TransferToAccount
  { transferToAccountTransferId :: UUID
  , transferToAccountSourceId :: UUID
  , transferToAccountAmount :: Double
  , transferToAccountTargetAccount :: UUID
  } deriving (Show, Eq)

data AcceptTransfer =
  AcceptTransfer
  { acceptTransferTransferId :: UUID
  , acceptTransferSourceId :: UUID
  , acceptTransferAmount :: Double
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "openAccount") ''OpenAccount
deriveJSON (unPrefixLower "creditAccount") ''CreditAccount
deriveJSON (unPrefixLower "debitAccount") ''DebitAccount
deriveJSON (unPrefixLower "transferToAccount") ''TransferToAccount
deriveJSON (unPrefixLower "acceptTransfer") ''AcceptTransfer

data AccountCommandError
  = AccountAlreadyOpenError
  | InvalidInitialDepositError
  | NotEnoughFundsError NotEnoughFundsData
  | AccountNotOwnedError
  deriving (Show, Eq)

data NotEnoughFundsData =
  NotEnoughFundsData
  { notEnoughFundsDataRemainingFunds :: Double
  } deriving  (Show, Eq)

deriveJSON (unPrefixLower "notEnoughFundsData") ''NotEnoughFundsData

deriveJSON defaultOptions ''AccountCommandError
