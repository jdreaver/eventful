module Bank.Aggregates.Account
  ( Account (..)
  , PendingAccountTransfer (..)
  , findAccountTransferById
  , AccountProjection
  , accountProjection
  , AccountCommand (..)
  , OpenAccountData (..)
  , CreditAccountData (..)
  , DebitAccountData (..)
  , TransferToAccountData (..)
  , AcceptTransferData (..)
  , AccountCommandError (..)
  , NotEnoughFundsData (..)
  , AccountAggregate
  , accountAggregate

  , accountAvailableBalance
  ) where

import Data.Aeson.TH
import Data.List (delete, find)
import Data.Maybe (isJust)

import Eventful

import Bank.Events
import Bank.Json

data Account =
  Account
  { accountBalance :: Double
  , accountOwner :: Maybe UUID
  , accountPendingTransfers :: [PendingAccountTransfer]
  } deriving (Show, Eq)

accountDefault :: Account
accountDefault = Account 0 Nothing []

data PendingAccountTransfer =
  PendingAccountTransfer
  { pendingAccountTransferId :: UUID
  , pendingAccountTransferSourceAccount :: UUID
  , pendingAccountTransferAmount :: Double
  , pendingAccountTransferTargetAccount :: UUID
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "account") ''Account
deriveJSON (unPrefixLower "pendingAccountTransfer") ''PendingAccountTransfer

-- | Account balance minus pending balance
accountAvailableBalance :: Account -> Double
accountAvailableBalance account = accountBalance account - pendingBalance
  where
    transfers = accountPendingTransfers account
    pendingBalance = if null transfers then 0 else sum (map pendingAccountTransferAmount transfers)

findAccountTransferById :: [PendingAccountTransfer] -> UUID -> Maybe PendingAccountTransfer
findAccountTransferById transfers transferId = find ((== transferId) . pendingAccountTransferId) transfers

applyAccountOpened :: Account -> AccountOpened -> Account
applyAccountOpened account (AccountOpened uuid amount) = account { accountOwner = Just uuid, accountBalance = amount }

applyAccountCredited :: Account -> AccountCredited -> Account
applyAccountCredited account (AccountCredited amount _) = account { accountBalance = accountBalance account + amount }

applyAccountDebited :: Account -> AccountDebited -> Account
applyAccountDebited account (AccountDebited amount _) = account { accountBalance = accountBalance account - amount }

applyAccountTransferStarted :: Account -> AccountTransferStarted -> Account
applyAccountTransferStarted account (AccountTransferStarted uuid sourceId amount targetId) =
  account { accountPendingTransfers = transfer : accountPendingTransfers account }
  where
    transfer = PendingAccountTransfer uuid sourceId amount targetId

applyAccountTransferCompleted :: Account -> AccountTransferCompleted -> Account
applyAccountTransferCompleted account (AccountTransferCompleted uuid) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (findAccountTransferById transfers uuid)
  where
    transfers = accountPendingTransfers account
    go trans =
      account
      { accountBalance = accountBalance account - pendingAccountTransferAmount trans
      , accountPendingTransfers = delete trans (accountPendingTransfers account)
      }

applyAccountTransferRejected :: Account -> AccountTransferRejected -> Account
applyAccountTransferRejected account (AccountTransferRejected uuid _) =
  account { accountPendingTransfers = transfers' }
  where
    transfers = accountPendingTransfers account
    transfers' = maybe transfers (flip delete transfers) (findAccountTransferById transfers uuid)

applyAccountCreditedFromTransfer :: Account -> AccountCreditedFromTransfer -> Account
applyAccountCreditedFromTransfer account (AccountCreditedFromTransfer _ _ amount) =
  account { accountBalance = accountBalance account + amount }

applyAccountEvent :: Account -> BankEvent -> Account
applyAccountEvent account (AccountOpenedEvent event) = applyAccountOpened account event
applyAccountEvent account (AccountCreditedEvent event) = applyAccountCredited account event
applyAccountEvent account (AccountDebitedEvent event) = applyAccountDebited account event
applyAccountEvent account (AccountTransferStartedEvent event) = applyAccountTransferStarted account event
applyAccountEvent account (AccountTransferCompletedEvent event) = applyAccountTransferCompleted account event
applyAccountEvent account (AccountTransferRejectedEvent event) = applyAccountTransferRejected account event
applyAccountEvent account (AccountCreditedFromTransferEvent event) = applyAccountCreditedFromTransfer account event
applyAccountEvent account _ = account

type AccountProjection = Projection Account BankEvent

accountProjection :: AccountProjection
accountProjection = Projection accountDefault applyAccountEvent

data AccountCommand
  = OpenAccount OpenAccountData
  | CreditAccount CreditAccountData
  | DebitAccount DebitAccountData
  | TransferToAccount TransferToAccountData
  | AcceptTransfer AcceptTransferData
  deriving (Show, Eq)

data OpenAccountData =
  OpenAccountData
  { openAccountDataOwner :: UUID
  , openAccountDataInitialFunding :: Double
  } deriving (Show, Eq)

data CreditAccountData =
  CreditAccountData
  { creditAccountDataAmount :: Double
  , creditAccountDataReason :: String
  } deriving (Show, Eq)

data DebitAccountData =
  DebitAccountData
  { debitAccountDataAmount :: Double
  , debitAccountDataReason :: String
  } deriving (Show, Eq)

data TransferToAccountData =
  TransferToAccountData
  { transferToAccountDataTransferId :: UUID
  , transferToAccountDataSourceId :: UUID
  , transferToAccountDataAmount :: Double
  , transferToAccountDataTargetAccount :: UUID
  } deriving (Show, Eq)

data AcceptTransferData =
  AcceptTransferData
  { acceptTransferTransferId :: UUID
  , acceptTransferSourceId :: UUID
  , acceptTransferDataAmount :: Double
  } deriving (Show, Eq)

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

applyAccountCommand :: Account -> AccountCommand -> Either AccountCommandError [BankEvent]
applyAccountCommand account (OpenAccount (OpenAccountData owner amount)) =
  case accountOwner account of
    Just _ -> Left AccountAlreadyOpenError
    Nothing ->
      if amount < 0
      then Left InvalidInitialDepositError
      else Right [AccountOpenedEvent $ AccountOpened owner amount]
applyAccountCommand _ (CreditAccount (CreditAccountData amount reason)) =
  Right [AccountCreditedEvent $ AccountCredited amount reason]
applyAccountCommand account (DebitAccount (DebitAccountData amount reason)) =
  if accountAvailableBalance account - amount < 0
  then Left $ NotEnoughFundsError (NotEnoughFundsData $ accountAvailableBalance account)
  else Right [AccountDebitedEvent $ AccountDebited amount reason]
applyAccountCommand account (TransferToAccount (TransferToAccountData uuid sourceId amount targetId)) =
  if accountAvailableBalance account - amount < 0
  then Left $ NotEnoughFundsError (NotEnoughFundsData $ accountAvailableBalance account)
  else Right [AccountTransferStartedEvent $ AccountTransferStarted uuid sourceId amount targetId]
applyAccountCommand account (AcceptTransfer (AcceptTransferData transferId sourceId amount)) =
  if isJust (accountOwner account)
  then Right [AccountCreditedFromTransferEvent $ AccountCreditedFromTransfer transferId sourceId amount]
  else Left AccountNotOwnedError

type AccountAggregate = Aggregate Account BankEvent AccountCommand AccountCommandError

accountAggregate :: AccountAggregate
accountAggregate = Aggregate applyAccountCommand accountProjection
