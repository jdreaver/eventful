module Bank.Aggregates.Account
  ( Account (..)
  , PendingAccountTransfer (..)
  , AccountEvent (..)
  , accountEventSerializer
  , AccountOpened (..)
  , AccountCredited (..)
  , AccountDebited (..)
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
import Data.List (delete, lookup)
import Data.Maybe (isJust)

import Eventful

import Bank.Events
import Bank.Json

data Account =
  Account
  { accountBalance :: Double
  , accountOwner :: Maybe UUID
  , accountPendingTransfers :: [(UUID, PendingAccountTransfer)]
  } deriving (Show, Eq)

data PendingAccountTransfer =
  PendingAccountTransfer
  { pendingAccountTransferAmount :: Double
  , pendingAccountTransferTargetAccount :: UUID
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "account") ''Account
deriveJSON (unPrefixLower "pendingAccountTransfer") ''PendingAccountTransfer

-- | Account balance minus pending balance
accountAvailableBalance :: Account -> Double
accountAvailableBalance account = accountBalance account - pendingBalance
  where
    transfers = map snd $ accountPendingTransfers account
    pendingBalance = if null transfers then 0 else sum (map pendingAccountTransferAmount transfers)

mkEventSumType' "AccountEvent"
  [ ''AccountOpened
  , ''AccountCredited
  , ''AccountDebited
  , ''AccountTransferStarted
  , ''AccountTransferCompleted
  , ''AccountTransferRejected
  , ''AccountCreditedFromTransfer
  ]
deriving instance Show AccountEvent
deriving instance Eq AccountEvent

mkSumTypeSerializer "accountEventSerializer" ''AccountEvent ''BankEvent

applyAccountEvent :: Account -> AccountEvent -> Account
applyAccountEvent account (AccountOpened' (AccountOpened uuid amount)) =
  account { accountOwner = Just uuid, accountBalance = amount }
applyAccountEvent account (AccountCredited' (AccountCredited amount _)) =
  account { accountBalance = accountBalance account + amount }
applyAccountEvent account (AccountDebited' (AccountDebited amount _)) =
  account { accountBalance = accountBalance account - amount }
applyAccountEvent account (AccountTransferStarted' (AccountTransferStarted uuid amount targetId)) =
  account { accountPendingTransfers = (uuid, transfer) : accountPendingTransfers account }
  where
    transfer = PendingAccountTransfer amount targetId
applyAccountEvent account (AccountTransferCompleted' (AccountTransferCompleted uuid)) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (lookup uuid (accountPendingTransfers account))
  where
    go trans@(PendingAccountTransfer amount _) =
      account
      { accountBalance = accountBalance account - amount
      , accountPendingTransfers = delete (uuid, trans) (accountPendingTransfers account)
      }
applyAccountEvent account (AccountTransferRejected' (AccountTransferRejected uuid _)) =
  account { accountPendingTransfers = transfers' }
  where
    transfers = accountPendingTransfers account
    transfers' = maybe transfers (\trans -> delete (uuid, trans) transfers) (lookup uuid transfers)
applyAccountEvent account (AccountCreditedFromTransfer' (AccountCreditedFromTransfer _ _ amount)) =
  account { accountBalance = accountBalance account + amount }

type AccountProjection = Projection Account AccountEvent

accountProjection :: AccountProjection
accountProjection =
  Projection
  (Account 0 Nothing [])
  applyAccountEvent

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

applyAccountCommand :: Account -> AccountCommand -> Either AccountCommandError [AccountEvent]
applyAccountCommand account (OpenAccount (OpenAccountData owner amount)) =
  case accountOwner account of
    Just _ -> Left AccountAlreadyOpenError
    Nothing ->
      if amount < 0
      then Left InvalidInitialDepositError
      else Right [AccountOpened' $ AccountOpened owner amount]
applyAccountCommand _ (CreditAccount (CreditAccountData amount reason)) =
  Right [AccountCredited' $ AccountCredited amount reason]
applyAccountCommand account (DebitAccount (DebitAccountData amount reason)) =
  if accountAvailableBalance account - amount < 0
  then Left $ NotEnoughFundsError (NotEnoughFundsData $ accountAvailableBalance account)
  else Right [AccountDebited' $ AccountDebited amount reason]
applyAccountCommand account (TransferToAccount (TransferToAccountData uuid amount targetId)) =
  if accountAvailableBalance account - amount < 0
  then Left $ NotEnoughFundsError (NotEnoughFundsData $ accountAvailableBalance account)
  else Right [AccountTransferStarted' $ AccountTransferStarted uuid amount targetId]
applyAccountCommand account (AcceptTransfer (AcceptTransferData transferId sourceId amount)) =
  if isJust (accountOwner account)
  then Right [AccountCreditedFromTransfer' $ AccountCreditedFromTransfer transferId sourceId amount]
  else Left AccountNotOwnedError

type AccountAggregate = Aggregate Account AccountEvent AccountCommand AccountCommandError

accountAggregate :: AccountAggregate
accountAggregate = Aggregate applyAccountCommand accountProjection
