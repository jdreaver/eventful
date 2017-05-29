{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.Commands
  ( accountCommands
  , OpenAccount (..)
  , CreditAccount (..)
  , DebitAccount (..)
  , TransferToAccount (..)
  , AcceptTransfer (..)
  ) where

import Language.Haskell.TH (Name)

import Eventful.UUID

import Bank.Json

accountCommands :: [Name]
accountCommands =
  [ ''OpenAccount
  , ''CreditAccount
  , ''DebitAccount
  , ''TransferToAccount
  , ''AcceptTransfer
  ]

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
  , transferToAccountSourceAccount :: UUID
  , transferToAccountAmount :: Double
  , transferToAccountTargetAccount :: UUID
  } deriving (Show, Eq)

data AcceptTransfer =
  AcceptTransfer
  { acceptTransferTransferId :: UUID
  , acceptTransferSourceAccount :: UUID
  , acceptTransferAmount :: Double
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''OpenAccount
deriveJSONUnPrefixLower ''CreditAccount
deriveJSONUnPrefixLower ''DebitAccount
deriveJSONUnPrefixLower ''TransferToAccount
deriveJSONUnPrefixLower ''AcceptTransfer
