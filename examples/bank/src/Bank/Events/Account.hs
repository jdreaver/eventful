module Bank.Events.Account
  ( AccountOpened (..)
  , AccountCredited (..)
  , AccountDebited (..)
  , AccountTransferStarted (..)
  , AccountTransferCompleted (..)
  , AccountTransferRejected (..)
  , AccountCreditedFromTransfer (..)
  ) where

import Data.Aeson.TH

import Eventful (UUID)

import Bank.Json

data AccountOpened =
  AccountOpened
  { accountOpenedOwner :: UUID
  , accountOpenedInitialFunding :: Double
  } deriving (Show, Eq)

data AccountCredited =
  AccountCredited
  { accountCreditedAmount :: Double
  , accountCreditedReason :: String
  } deriving (Show, Eq)

data AccountDebited =
  AccountDebited
  { accountDebitedAmount :: Double
  , accountDebitedReason :: String
  } deriving (Show, Eq)

data AccountTransferStarted =
  AccountTransferStarted
  { accountTransferStartedTransferId :: UUID
  , accountTransferStartedAmount :: Double
  , accountTransferStartedTargetAccount :: UUID
  } deriving (Show, Eq)

data AccountTransferCompleted =
  AccountTransferCompleted
  { accountTransferCompletedTransferId :: UUID
  } deriving (Show, Eq)

data AccountTransferRejected =
  AccountTransferRejected
  { accountTransferRejectedTransferId :: UUID
  , accountTransferRejectedReason :: String
  } deriving (Show, Eq)

data AccountCreditedFromTransfer =
  AccountCreditedFromTransfer
  { accountCreditedFromTransferTransferId :: UUID
  , accountCreditedFromTransferSourceAccount :: UUID
  , accountCreditedFromTransferAmount :: Double
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "accountOpened") ''AccountOpened
deriveJSON (unPrefixLower "accountCredited") ''AccountCredited
deriveJSON (unPrefixLower "accountDebited") ''AccountDebited
deriveJSON (unPrefixLower "accountTransferStarted") ''AccountTransferStarted
deriveJSON (unPrefixLower "accountTransferCompleted") ''AccountTransferCompleted
deriveJSON (unPrefixLower "accountTransferRejected") ''AccountTransferRejected
deriveJSON (unPrefixLower "accountCreditedFromTransfer") ''AccountCreditedFromTransfer
