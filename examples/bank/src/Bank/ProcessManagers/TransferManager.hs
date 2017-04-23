module Bank.ProcessManagers.TransferManager
  ( TransferManager (..)
  , TransferManagerTransferData (..)
  , TransferProcessManager
  , transferProcessManager
  ) where

import Eventful

import Bank.Aggregates.Account
import Bank.Events

data TransferManager
  = TransferManager
  { transferManagerData :: Maybe TransferManagerTransferData
  , transferManagerPendingCommands :: [(UUID, AccountCommand)]
  , transferManagerPendingEvents :: [(UUID, AccountEvent)]
  } deriving (Show, Eq)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferId :: UUID
  , transferSourceId :: UUID
  , transferTargetId :: UUID
  } deriving (Show, Eq)

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Nothing [] []

type TransferManagerProjection = Projection TransferManager AccountEvent

transferManagerProjection :: TransferManagerProjection
transferManagerProjection =
  Projection
  transferManagerDefault
  applyAccountEvent

applyAccountEvent :: TransferManager -> AccountEvent -> TransferManager
applyAccountEvent manager (AccountAccountTransferStarted event) = applyAccountTransferStarted manager event
applyAccountEvent manager (AccountAccountCreditedFromTransfer event) = applyAccountCreditedFromTransfer manager event
applyAccountEvent manager _ = manager

applyAccountTransferStarted :: TransferManager -> AccountTransferStarted -> TransferManager
applyAccountTransferStarted manager (AccountTransferStarted transferId sourceId amount targetId) =
  manager
  { transferManagerData = Just (TransferManagerTransferData transferId sourceId targetId)
  , transferManagerPendingCommands = [(targetId, AcceptTransfer (AcceptTransferData transferId sourceId amount))]
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
      [(sourceId, AccountAccountTransferCompleted $ AccountTransferCompleted transferId)]

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
      [(sourceId, AccountAccountTransferRejected $ AccountTransferRejected transferId "Rejected in transfer saga")]

type TransferProcessManager = ProcessManager TransferManager AccountEvent AccountCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
  transferManagerProjection
  cancelTransfer
  transferManagerPendingCommands
  transferManagerPendingEvents
