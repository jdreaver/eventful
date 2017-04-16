module Bank.Events
  ( BankEvent (..)
  , module X
  ) where

import Data.Aeson.TH
import GHC.Generics

import Eventful

import Bank.Events.Account as X
import Bank.Events.Customer as X
import Bank.Json

-- | This ADT is the definitive collection of all possible events in the
-- system. The reason we put them in a single ADT is so we have a consistent
-- serialization format for each events that are shared across
-- projections/aggregates.
data BankEvent
  -- Account events
  = AccountOpenedEvent AccountOpened
  | AccountCreditedEvent AccountCredited
  | AccountDebitedEvent AccountDebited
  | AccountTransferStartedEvent AccountTransferStarted
  | AccountTransferCompletedEvent AccountTransferCompleted
  | AccountTransferRejectedEvent AccountTransferRejected

  -- Customer events
  | CustomerCreatedEvent CustomerCreated
  deriving (Show, Eq, Generic)

instance EventSumType BankEvent

deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "Event" }) ''BankEvent
