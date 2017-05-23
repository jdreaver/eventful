{-# LANGUAGE TemplateHaskell #-}

module Bank.Commands.Account
  ( OpenAccount (..)
  , CreditAccount (..)
  , DebitAccount (..)
  , TransferToAccount (..)
  , AcceptTransfer (..)
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

deriveJSON (unPrefixLower "openAccount") ''OpenAccount
deriveJSON (unPrefixLower "creditAccount") ''CreditAccount
deriveJSON (unPrefixLower "debitAccount") ''DebitAccount
deriveJSON (unPrefixLower "transferToAccount") ''TransferToAccount
deriveJSON (unPrefixLower "acceptTransfer") ''AcceptTransfer
