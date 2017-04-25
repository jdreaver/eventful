module Bank.Aggregates.Account
  ( Account (..)
  , PendingAccountTransfer (..)
  , findAccountTransferById
  , accountProjection
  , accountAggregate

  , accountAvailableBalance
  ) where

import Data.Aeson.TH
import Data.List (delete, find)
import Data.Maybe (isJust)

import Eventful

import Bank.Commands
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
applyAccountEvent account (AccountOpened' event) = applyAccountOpened account event
applyAccountEvent account (AccountCredited' event) = applyAccountCredited account event
applyAccountEvent account (AccountDebited' event) = applyAccountDebited account event
applyAccountEvent account (AccountTransferStarted' event) = applyAccountTransferStarted account event
applyAccountEvent account (AccountTransferCompleted' event) = applyAccountTransferCompleted account event
applyAccountEvent account (AccountTransferRejected' event) = applyAccountTransferRejected account event
applyAccountEvent account (AccountCreditedFromTransfer' event) = applyAccountCreditedFromTransfer account event
applyAccountEvent account _ = account

accountProjection :: BankProjection Account
accountProjection = Projection accountDefault applyAccountEvent

applyAccountCommand :: Account -> BankCommand -> [BankEvent]
applyAccountCommand account (OpenAccount' (OpenAccount owner amount)) =
  case accountOwner account of
    Just _ -> [AccountOpenRejected' $ AccountOpenRejected "Account already open"]
    Nothing ->
      if amount < 0
      then [AccountOpenRejected' $ AccountOpenRejected "Invalid initial deposit"]
      else [AccountOpened' $ AccountOpened owner amount]
applyAccountCommand _ (CreditAccount' (CreditAccount amount reason)) =
  [AccountCredited' $ AccountCredited amount reason]
applyAccountCommand account (DebitAccount' (DebitAccount amount reason)) =
  if accountAvailableBalance account - amount < 0
  then [AccountDebitRejected' $ AccountDebitRejected $ accountAvailableBalance account]
  else [AccountDebited' $ AccountDebited amount reason]
applyAccountCommand account (TransferToAccount' (TransferToAccount uuid sourceId amount targetId))
  | isJust (accountOwner account) = [AccountTransferRejected' $ AccountTransferRejected uuid "Account doesn't exist"]
  | accountAvailableBalance account - amount < 0 = [AccountTransferRejected' $ AccountTransferRejected uuid "Not enough funds"]
  | otherwise = [AccountTransferStarted' $ AccountTransferStarted uuid sourceId amount targetId]
applyAccountCommand _ (AcceptTransfer' (AcceptTransfer transferId sourceId amount)) =
  [AccountCreditedFromTransfer' $ AccountCreditedFromTransfer transferId sourceId amount]
applyAccountCommand _ _ = []

accountAggregate :: BankAggregate Account
accountAggregate = Aggregate applyAccountCommand accountProjection
