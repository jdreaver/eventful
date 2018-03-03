{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module BankCommon.Models.Account.CommandHandler
  ( accountCommandHandler
  , AccountCommand (..)
  ) where


import Control.Lens
import Data.Maybe (isNothing)
import SumTypes.TH

import Eventful

import BankCommon.Models.Account.Commands
import BankCommon.Models.Account.Events
import BankCommon.Models.Account.Projection

constructSumType "AccountCommand" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) accountCommands

handleAccountCommand :: Account -> AccountCommand -> [AccountEvent]
handleAccountCommand account (OpenAccountAccountCommand OpenAccount{..}) =
  case account ^. accountOwner of
    Just _ -> [AccountOpenRejectedAccountEvent $ AccountOpenRejected "Account already open"]
    Nothing ->
      if openAccountInitialFunding < 0
      then [AccountOpenRejectedAccountEvent $ AccountOpenRejected "Invalid initial deposit"]
      else
        [ AccountOpenedAccountEvent
          AccountOpened
          { accountOpenedOwner = openAccountOwner
          , accountOpenedInitialFunding = openAccountInitialFunding
          }
        ]
handleAccountCommand _ (CreditAccountAccountCommand CreditAccount{..}) =
  [ AccountCreditedAccountEvent
    AccountCredited
    { accountCreditedAmount = creditAccountAmount
    , accountCreditedReason = creditAccountReason
    }
  ]
handleAccountCommand account (DebitAccountAccountCommand DebitAccount{..}) =
  if accountAvailableBalance account - debitAccountAmount < 0
  then [AccountDebitRejectedAccountEvent $ AccountDebitRejected $ accountAvailableBalance account]
  else
    [ AccountDebitedAccountEvent
      AccountDebited
      { accountDebitedAmount = debitAccountAmount
      , accountDebitedReason = debitAccountReason
      }
    ]
handleAccountCommand account (TransferToAccountAccountCommand TransferToAccount{..})
  | isNothing (account ^. accountOwner) =
      [AccountTransferRejectedAccountEvent $ AccountTransferRejected transferToAccountTransferId "Account doesn't exist"]
  | accountAvailableBalance account - transferToAccountAmount < 0 =
      [AccountTransferRejectedAccountEvent $ AccountTransferRejected transferToAccountTransferId "Not enough funds"]
  | otherwise =
      [ AccountTransferStartedAccountEvent
        AccountTransferStarted
        { accountTransferStartedTransferId = transferToAccountTransferId
        , accountTransferStartedAmount = transferToAccountAmount
        , accountTransferStartedTargetAccount = transferToAccountTargetAccount
        }
      ]
handleAccountCommand _ (AcceptTransferAccountCommand AcceptTransfer{..}) =
  [ AccountCreditedFromTransferAccountEvent
    AccountCreditedFromTransfer
    { accountCreditedFromTransferTransferId = acceptTransferTransferId
    , accountCreditedFromTransferSourceAccount = acceptTransferSourceAccount
    , accountCreditedFromTransferAmount = acceptTransferAmount
    }
  ]

accountCommandHandler :: CommandHandler Account AccountEvent AccountCommand
accountCommandHandler = CommandHandler handleAccountCommand accountProjection
