module Bank.ProcessManagers.TransferManager
  ( TransferManager (..)
  , TransferManagerTransferData (..)
  , TransferProcessManager
  , transferProcessManager
  ) where

import Eventful

import Bank.Commands
import Bank.Events

data TransferManager
  = TransferManager
  { transferManagerData :: Maybe TransferManagerTransferData
  , transferManagerPendingCommands :: [(UUID, BankCommand)]
  , transferManagerPendingEvents :: [(UUID, BankEvent)]
  } deriving (Show, Eq)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferId :: UUID
  , transferSourceId :: UUID
  , transferTargetId :: UUID
  } deriving (Show, Eq)

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Nothing [] []

transferManagerProjection :: BankProjection TransferManager
transferManagerProjection =
  Projection
  transferManagerDefault
  applyAccountEvent

applyAccountEvent :: TransferManager -> BankEvent -> TransferManager
applyAccountEvent manager (AccountTransferStarted' event) = applyAccountTransferStarted manager event
applyAccountEvent manager (AccountTransferRejected' _) = cancelTransfer manager
applyAccountEvent manager (AccountCreditedFromTransfer' event) = applyAccountCreditedFromTransfer manager event
applyAccountEvent manager _ = manager

applyAccountTransferStarted :: TransferManager -> AccountTransferStarted -> TransferManager
applyAccountTransferStarted manager (AccountTransferStarted transferId sourceId amount targetId) =
  manager
  { transferManagerData = Just (TransferManagerTransferData transferId sourceId targetId)
  , transferManagerPendingCommands = [(targetId, AcceptTransfer' (AcceptTransfer transferId sourceId amount))]
  , transferManagerPendingEvents = []
  }

applyAccountCreditedFromTransfer :: TransferManager -> AccountCreditedFromTransfer -> TransferManager
applyAccountCreditedFromTransfer manager AccountCreditedFromTransfer{} =
  manager
  { transferManagerPendingCommands = []
  , transferManagerPendingEvents = events
  }
  where
    events = maybe [] mkEvent (transferManagerData manager)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      [(sourceId, AccountTransferCompleted' $ AccountTransferCompleted transferId)]

cancelTransfer :: TransferManager -> TransferManager
cancelTransfer manager =
  manager
  { transferManagerPendingCommands = []
  , transferManagerPendingEvents = events
  }
  where
    events = maybe [] mkEvent (transferManagerData manager)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      -- TODO: Find a way to get the actual error so we can put it in this
      -- event.
      [(sourceId, AccountTransferRejected' $ AccountTransferRejected transferId "Rejected in transfer saga")]

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
  transferManagerProjection
  transferManagerPendingCommands
  transferManagerPendingEvents
