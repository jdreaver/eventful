{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.ProcessManagers.TransferManager
  ( TransferManager (..)
  , TransferManagerTransferData (..)
  , TransferProcessManager
  , transferProcessManager
  ) where

import Control.Lens
import Data.Maybe (isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Eventful

import Bank.Models

data TransferManager
  = TransferManager
  { _transferManagerData :: Map UUID TransferManagerTransferData
  , _transferManagerCanceled :: Bool
  , _transferManagerPendingCommands :: [ProcessManagerCommand BankEvent BankCommand]
  , _transferManagerPendingEvents :: [StreamEvent UUID () BankEvent]
  } deriving (Show)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferSourceAccount :: UUID
  , transferTargetAccount :: UUID
  } deriving (Show, Eq)

makeLenses ''TransferManager

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Map.empty False [] []

transferManagerProjection :: Projection TransferManager (VersionedStreamEvent BankEvent)
transferManagerProjection =
  Projection
  transferManagerDefault
  handleAccountEvent

handleAccountEvent :: TransferManager -> VersionedStreamEvent BankEvent -> TransferManager
handleAccountEvent manager (StreamEvent sourceAccount _ (AccountTransferStartedEvent AccountTransferStarted{..})) =
  manager
  & transferManagerData . at accountTransferStartedTransferId ?~
    TransferManagerTransferData
    { transferSourceAccount = sourceAccount
    , transferTargetAccount = accountTransferStartedTargetAccount
    }
  & transferManagerPendingCommands .~ (
      if isNothing (manager ^. transferManagerData . at accountTransferStartedTransferId)
      then [ProcessManagerCommand accountTransferStartedTargetAccount accountBankCommandHandler command]
      else []
    )
  & transferManagerPendingEvents .~ []
  where
    command =
      AcceptTransferCommand
      AcceptTransfer
      { acceptTransferTransferId = accountTransferStartedTransferId
      , acceptTransferSourceAccount = sourceAccount
      , acceptTransferAmount = accountTransferStartedAmount
      }
handleAccountEvent manager (StreamEvent _ _ (AccountTransferRejectedEvent AccountTransferRejected{..})) =
  manager
  & transferManagerCanceled .~ True
  & transferManagerPendingCommands .~ []
  & transferManagerPendingEvents .~ if manager ^. transferManagerCanceled then [] else events
  where
    events = maybe [] mkEvent (manager ^. transferManagerData . at accountTransferRejectedTransferId)
    mkEvent (TransferManagerTransferData sourceId _) =
      -- TODO: Find a way to get the actual error so we can put it in this
      -- event.
      [StreamEvent sourceId () $
       AccountTransferRejectedEvent $ AccountTransferRejected accountTransferRejectedTransferId "Rejected in transfer saga"]
handleAccountEvent manager (StreamEvent _ _ (AccountCreditedFromTransferEvent AccountCreditedFromTransfer{..})) =
  manager
  & transferManagerPendingCommands .~ []
  & transferManagerPendingEvents .~ events
  where
    events = maybe [] mkEvent (manager ^. transferManagerData . at accountCreditedFromTransferTransferId)
    mkEvent (TransferManagerTransferData sourceId _) =
      [StreamEvent sourceId () $ AccountTransferCompletedEvent $ AccountTransferCompleted accountCreditedFromTransferTransferId]
handleAccountEvent manager _ =
  manager
  & transferManagerPendingCommands .~ []
  & transferManagerPendingEvents .~ []

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
  transferManagerProjection
  (view transferManagerPendingCommands)
  (view transferManagerPendingEvents)
