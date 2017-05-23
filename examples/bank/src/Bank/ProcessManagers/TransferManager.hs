{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.ProcessManagers.TransferManager
  ( TransferManager (..)
  , TransferManagerTransferData (..)
  , TransferProcessManager
  , transferProcessManager
  , transferManagerRouter
  ) where

import Control.Lens
import Data.Maybe (isNothing)

import Eventful

import Bank.Aggregates.Account
import Bank.Commands
import Bank.Events

data TransferManager
  = TransferManager
  { _transferManagerData :: Maybe TransferManagerTransferData
  , _transferManagerCanceled :: Bool
  , _transferManagerPendingCommands :: [ProcessManagerCommand BankEvent BankCommand]
  , _transferManagerPendingEvents :: [ProcessManagerEvent BankEvent]
  } deriving (Show)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferId :: UUID
  , transferSourceAccount :: UUID
  , transferTargetAccount :: UUID
  } deriving (Show, Eq)

makeLenses ''TransferManager

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Nothing False [] []

transferManagerProjection :: BankProjection TransferManager
transferManagerProjection =
  Projection
  transferManagerDefault
  handleAccountEvent

handleAccountEvent :: TransferManager -> BankEvent -> TransferManager
handleAccountEvent manager (AccountTransferStartedEvent AccountTransferStarted{..}) =
  manager
  & transferManagerData ?~
    TransferManagerTransferData
    { transferId = accountTransferStartedTransferId
    , transferSourceAccount = accountTransferStartedSourceAccount
    , transferTargetAccount = accountTransferStartedTargetAccount
    }
  & transferManagerPendingCommands .~ (
      if isNothing (manager ^. transferManagerData )
      then [ProcessManagerCommand accountTransferStartedTargetAccount accountAggregate command]
      else []
    )
  & transferManagerPendingEvents .~ []
  where
    command =
      AcceptTransferCommand
      AcceptTransfer
      { acceptTransferTransferId = accountTransferStartedTransferId
      , acceptTransferSourceAccount = accountTransferStartedSourceAccount
      , acceptTransferAmount = accountTransferStartedAmount
      }
handleAccountEvent manager (AccountTransferRejectedEvent _) =
  manager
  & transferManagerCanceled .~ True
  & transferManagerPendingCommands .~ []
  & transferManagerPendingEvents .~ if manager ^. transferManagerCanceled then [] else events
  where
    events = maybe [] mkEvent (manager ^. transferManagerData)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      -- TODO: Find a way to get the actual error so we can put it in this
      -- event.
      [ProcessManagerEvent sourceId $
       AccountTransferRejectedEvent $ AccountTransferRejected transferId "Rejected in transfer saga"]
handleAccountEvent manager (AccountCreditedFromTransferEvent AccountCreditedFromTransfer{..}) =
  manager
  & transferManagerPendingCommands .~ []
  & transferManagerPendingEvents .~ events
  where
    events = maybe [] mkEvent (manager ^. transferManagerData)
    mkEvent (TransferManagerTransferData transferId sourceId _) =
      [ProcessManagerEvent sourceId $ AccountTransferCompletedEvent $ AccountTransferCompleted transferId]
handleAccountEvent manager _ = manager

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
  transferManagerProjection
  (view transferManagerPendingCommands)
  (view transferManagerPendingEvents)

transferManagerRouter :: ProcessManagerRouter TransferManager BankEvent BankCommand
transferManagerRouter = ProcessManagerRouter routeTransferManager transferProcessManager

routeTransferManager :: UUID -> BankEvent -> Maybe UUID
routeTransferManager eventId (AccountTransferStartedEvent AccountTransferStarted{..}) =
  listenIfUuidsDifferent eventId accountTransferStartedTransferId
routeTransferManager eventId (AccountTransferRejectedEvent AccountTransferRejected{..}) =
  listenIfUuidsDifferent eventId accountTransferRejectedTransferId
routeTransferManager eventId (AccountCreditedFromTransferEvent AccountCreditedFromTransfer{..}) =
  listenIfUuidsDifferent eventId accountCreditedFromTransferTransferId
routeTransferManager _ _ = Nothing

listenIfUuidsDifferent :: UUID -> UUID -> Maybe UUID
listenIfUuidsDifferent eventId transferId =
  if eventId == transferId
  then Nothing
  else Just transferId
