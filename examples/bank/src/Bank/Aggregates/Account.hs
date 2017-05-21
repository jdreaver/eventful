module Bank.Aggregates.Account
  ( Account (..)
  , accountBalance
  , accountOwner
  , accountPendingTransfers
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

data Account
  = Account
  { _accountBalance :: Double
  , _accountOwner :: Maybe UUID
  , _accountPendingTransfers :: [PendingAccountTransfer]
  } deriving (Show, Eq)

accountDefault :: Account
accountDefault = Account 0 Nothing []

data PendingAccountTransfer
  = PendingAccountTransfer
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

handleAccountEvent :: Account -> BankEvent -> Account
handleAccountEvent account (AccountOpenedEvent AccountOpened{..}) =
  account
  & accountOwner ?~ accountOpenedOwner
  & accountBalance .~ accountOpenedInitialFunding
handleAccountEvent account (AccountCreditedEvent AccountCredited{..}) =
  account
  & accountBalance +~ accountCreditedAmount
handleAccountEvent account (AccountDebitedEvent AccountDebited{..}) =
  account
  & accountBalance -~ accountDebitedAmount
handleAccountEvent account (AccountTransferStartedEvent AccountTransferStarted{..}) =
  account
  & accountPendingTransfers %~
    cons
    PendingAccountTransfer
    { pendingAccountTransferId = accountTransferStartedTransferId
    , pendingAccountTransferSourceAccount = accountTransferStartedSourceAccount
    , pendingAccountTransferAmount = accountTransferStartedAmount
    , pendingAccountTransferTargetAccount = accountTransferStartedTargetAccount
    }
handleAccountEvent account (AccountTransferCompletedEvent AccountTransferCompleted{..}) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (findAccountTransferById transfers accountTransferCompletedTransferId)
  where
    transfers = account ^. accountPendingTransfers
    go trans =
      account
      & accountBalance -~ pendingAccountTransferAmount trans
      & accountPendingTransfers %~ delete trans
handleAccountEvent account (AccountTransferRejectedEvent AccountTransferRejected{..}) =
  account
  & accountPendingTransfers .~ transfers'
  where
    transfers = account ^. accountPendingTransfers
    mTransfer = findAccountTransferById transfers accountTransferRejectedTransferId
    transfers' = maybe transfers (flip delete transfers) mTransfer
handleAccountEvent account (AccountCreditedFromTransferEvent AccountCreditedFromTransfer{..}) =
  account
  & accountBalance +~ accountCreditedFromTransferAmount
handleAccountEvent account _ = account

accountProjection :: BankProjection Account
accountProjection = Projection accountDefault handleAccountEvent

handleAccountCommand :: Account -> BankCommand -> [BankEvent]
handleAccountCommand account (OpenAccountCommand OpenAccount{..}) =
  case account ^. accountOwner of
    Just _ -> [AccountOpenRejectedEvent $ AccountOpenRejected "Account already open"]
    Nothing ->
      if openAccountInitialFunding < 0
      then [AccountOpenRejectedEvent $ AccountOpenRejected "Invalid initial deposit"]
      else
        [ AccountOpenedEvent
          AccountOpened
          { accountOpenedOwner = openAccountOwner
          , accountOpenedInitialFunding = openAccountInitialFunding
          }
        ]
handleAccountCommand _ (CreditAccountCommand CreditAccount{..}) =
  [ AccountCreditedEvent
    AccountCredited
    { accountCreditedAmount = creditAccountAmount
    , accountCreditedReason = creditAccountReason
    }
  ]
handleAccountCommand account (DebitAccountCommand DebitAccount{..}) =
  if accountAvailableBalance account - debitAccountAmount < 0
  then [AccountDebitRejectedEvent $ AccountDebitRejected $ accountAvailableBalance account]
  else
    [ AccountDebitedEvent
      AccountDebited
      { accountDebitedAmount = debitAccountAmount
      , accountDebitedReason = debitAccountReason
      }
    ]
handleAccountCommand account (TransferToAccountCommand TransferToAccount{..})
  | isNothing (account ^. accountOwner) =
      [AccountTransferRejectedEvent $ AccountTransferRejected transferToAccountTransferId "Account doesn't exist"]
  | accountAvailableBalance account - transferToAccountAmount < 0 =
      [AccountTransferRejectedEvent $ AccountTransferRejected transferToAccountTransferId "Not enough funds"]
  | otherwise =
      [ AccountTransferStartedEvent
        AccountTransferStarted
        { accountTransferStartedTransferId = transferToAccountTransferId
        , accountTransferStartedSourceAccount = transferToAccountSourceAccount
        , accountTransferStartedAmount = transferToAccountAmount
        , accountTransferStartedTargetAccount = transferToAccountTargetAccount
        }
      ]
handleAccountCommand _ (AcceptTransferCommand AcceptTransfer{..}) =
  [ AccountCreditedFromTransferEvent
    AccountCreditedFromTransfer
    { accountCreditedFromTransferTransferId = acceptTransferTransferId
    , accountCreditedFromTransferSourceAccount = acceptTransferSourceAccount
    , accountCreditedFromTransferAmount = acceptTransferAmount
    }
  ]
handleAccountCommand _ _ = []

accountAggregate :: BankAggregate Account
accountAggregate = Aggregate handleAccountCommand accountProjection
