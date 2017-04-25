module Bank.ProcessManagers.TransferManager
  ( TransferManager (..)
  , TransferManagerTransferData (..)
  , TransferProcessManager
  , transferProcessManager
  , transferManagerRouter
  ) where

import Data.Maybe (isNothing)

import Eventful

import Bank.Aggregates.Account
import Bank.Commands
import Bank.Events

data TransferManager
  = TransferManager
  { transferManagerData :: Maybe TransferManagerTransferData
  , transferManagerCanceled :: Bool
  , transferManagerPendingCommands :: [ProcessManagerCommand BankEvent BankCommand]
  , transferManagerPendingEvents :: [ProcessManagerEvent BankEvent]
  } deriving (Show)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferId :: UUID
  , transferSourceId :: UUID
  , transferTargetId :: UUID
  } deriving (Show, Eq)

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Nothing False [] []

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
  , transferManagerPendingCommands =
      if isNothing (transferManagerData manager)
      then [ProcessManagerCommand targetId accountAggregate command]
      else []
  , transferManagerPendingEvents = []
  }
  where
    command = AcceptTransfer' (AcceptTransfer transferId sourceId amount)

applyAccountCreditedFromTransfer :: TransferManager -> AccountCreditedFromTransfer -> TransferManager
applyAccountCreditedFromTransfer manager AccountCreditedFromTransfer{} =
  manager
  { transferManagerPendingCommands = []
  , transferManagerPendingEvents = events
  }
  where
    events = maybe [] mkEvent (transferManagerData manager)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      [ProcessManagerEvent sourceId $ AccountTransferCompleted' $ AccountTransferCompleted transferId]

cancelTransfer :: TransferManager -> TransferManager
cancelTransfer manager =
  manager
  { transferManagerCanceled = True
  , transferManagerPendingCommands = []
  , transferManagerPendingEvents = if transferManagerCanceled manager then [] else events
  }
  where
    events = maybe [] mkEvent (transferManagerData manager)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      -- TODO: Find a way to get the actual error so we can put it in this
      -- event.
      [ProcessManagerEvent sourceId $
       AccountTransferRejected' $ AccountTransferRejected transferId "Rejected in transfer saga"]

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
  transferManagerProjection
  transferManagerPendingCommands
  transferManagerPendingEvents

transferManagerRouter :: ProcessManagerRouter TransferManager BankEvent BankCommand
transferManagerRouter = ProcessManagerRouter routeTransferManager transferProcessManager

routeTransferManager :: UUID -> BankEvent -> Maybe UUID
routeTransferManager eventId (AccountTransferStarted' AccountTransferStarted{..}) =
  listenIfUuidsDifferent eventId accountTransferStartedTransferId
routeTransferManager eventId (AccountTransferRejected' AccountTransferRejected{..}) =
  listenIfUuidsDifferent eventId accountTransferRejectedTransferId
routeTransferManager eventId (AccountCreditedFromTransfer' AccountCreditedFromTransfer{..}) =
  listenIfUuidsDifferent eventId accountCreditedFromTransferTransferId
routeTransferManager _ _ = Nothing

listenIfUuidsDifferent :: UUID -> UUID -> Maybe UUID
listenIfUuidsDifferent eventId transferId =
  if eventId == transferId
  then Nothing
  else Just transferId
