module Bank.Aggregates.Account
  ( Account (..)
  , PendingAccountTransfer (..)
  , findAccountTransferById
  , accountProjection
  , accountAggregate

  , accountAvailableBalance
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.List (delete, find)
import Data.Maybe (isNothing)

import Eventful

import Bank.Commands
import Bank.Events
import Bank.Json

data Account =
  Account
  { _accountBalance :: Double
  , _accountOwner :: Maybe UUID
  , _accountPendingTransfers :: [PendingAccountTransfer]
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

makeLenses ''Account
deriveJSON (unPrefixLower "_account") ''Account
deriveJSON (unPrefixLower "pendingAccountTransfer") ''PendingAccountTransfer

-- | Account balance minus pending balance
accountAvailableBalance :: Account -> Double
accountAvailableBalance account = account ^. accountBalance - pendingBalance
  where
    transfers = account ^. accountPendingTransfers
    pendingBalance = if null transfers then 0 else sum (map pendingAccountTransferAmount transfers)

findAccountTransferById :: [PendingAccountTransfer] -> UUID -> Maybe PendingAccountTransfer
findAccountTransferById transfers transferId = find ((== transferId) . pendingAccountTransferId) transfers

handleAccountOpened :: Account -> AccountOpened -> Account
handleAccountOpened account (AccountOpened uuid amount) =
  account
  & accountOwner ?~ uuid
  & accountBalance .~ amount

handleAccountCredited :: Account -> AccountCredited -> Account
handleAccountCredited account (AccountCredited amount _) =
  account
  & accountBalance +~ amount

handleAccountDebited :: Account -> AccountDebited -> Account
handleAccountDebited account (AccountDebited amount _) =
  account
  & accountBalance -~ amount

handleAccountTransferStarted :: Account -> AccountTransferStarted -> Account
handleAccountTransferStarted account (AccountTransferStarted uuid sourceId amount targetId) =
  account
  & accountPendingTransfers %~ cons (PendingAccountTransfer uuid sourceId amount targetId)

handleAccountTransferCompleted :: Account -> AccountTransferCompleted -> Account
handleAccountTransferCompleted account (AccountTransferCompleted uuid) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (findAccountTransferById transfers uuid)
  where
    transfers = account ^. accountPendingTransfers
    go trans =
      account
      & accountBalance -~ pendingAccountTransferAmount trans
      & accountPendingTransfers %~ delete trans

handleAccountTransferRejected :: Account -> AccountTransferRejected -> Account
handleAccountTransferRejected account (AccountTransferRejected uuid _) =
  account
  & accountPendingTransfers .~ transfers'
  where
    transfers = account ^. accountPendingTransfers
    transfers' = maybe transfers (flip delete transfers) (findAccountTransferById transfers uuid)

handleAccountCreditedFromTransfer :: Account -> AccountCreditedFromTransfer -> Account
handleAccountCreditedFromTransfer account (AccountCreditedFromTransfer _ _ amount) =
  account
  & accountBalance +~ amount

handleAccountEvent :: Account -> BankEvent -> Account
handleAccountEvent account (AccountOpened' event) = handleAccountOpened account event
handleAccountEvent account (AccountCredited' event) = handleAccountCredited account event
handleAccountEvent account (AccountDebited' event) = handleAccountDebited account event
handleAccountEvent account (AccountTransferStarted' event) = handleAccountTransferStarted account event
handleAccountEvent account (AccountTransferCompleted' event) = handleAccountTransferCompleted account event
handleAccountEvent account (AccountTransferRejected' event) = handleAccountTransferRejected account event
handleAccountEvent account (AccountCreditedFromTransfer' event) = handleAccountCreditedFromTransfer account event
handleAccountEvent account _ = account

accountProjection :: BankProjection Account
accountProjection = Projection accountDefault handleAccountEvent

handleAccountCommand :: Account -> BankCommand -> [BankEvent]
handleAccountCommand account (OpenAccount' (OpenAccount owner amount)) =
  case account ^. accountOwner of
    Just _ -> [AccountOpenRejected' $ AccountOpenRejected "Account already open"]
    Nothing ->
      if amount < 0
      then [AccountOpenRejected' $ AccountOpenRejected "Invalid initial deposit"]
      else [AccountOpened' $ AccountOpened owner amount]
handleAccountCommand _ (CreditAccount' (CreditAccount amount reason)) =
  [AccountCredited' $ AccountCredited amount reason]
handleAccountCommand account (DebitAccount' (DebitAccount amount reason)) =
  if accountAvailableBalance account - amount < 0
  then [AccountDebitRejected' $ AccountDebitRejected $ accountAvailableBalance account]
  else [AccountDebited' $ AccountDebited amount reason]
handleAccountCommand account (TransferToAccount' (TransferToAccount uuid sourceId amount targetId))
  | isNothing (account ^. accountOwner) = [AccountTransferRejected' $ AccountTransferRejected uuid "Account doesn't exist"]
  | accountAvailableBalance account - amount < 0 = [AccountTransferRejected' $ AccountTransferRejected uuid "Not enough funds"]
  | otherwise = [AccountTransferStarted' $ AccountTransferStarted uuid sourceId amount targetId]
handleAccountCommand _ (AcceptTransfer' (AcceptTransfer transferId sourceId amount)) =
  [AccountCreditedFromTransfer' $ AccountCreditedFromTransfer transferId sourceId amount]
handleAccountCommand _ _ = []

accountAggregate :: BankAggregate Account
accountAggregate = Aggregate handleAccountCommand accountProjection
