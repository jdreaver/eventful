module Bank.Events
  ( BankEvent (..)
  , BankProjection
  , module X
  ) where

import Data.Aeson.TH

import Eventful
import Eventful.TH

import Bank.Events.Account as X
import Bank.Events.Customer as X
import Bank.Json

-- | This ADT is the definitive collection of all possible events in the
-- system. The reason we put them in a single ADT is so we have a consistent
-- serialization format for each events that are shared across
-- projections/aggregates.
mkSumType "BankEvent" (++ "'")
  [ -- Account events
    ''AccountOpened
  , ''AccountCredited
  , ''AccountDebited
  , ''AccountTransferStarted
  , ''AccountTransferCompleted
  , ''AccountTransferRejected
  , ''AccountCreditedFromTransfer

  -- Customer events
  ,  ''CustomerCreated
  ]

deriving instance Show BankEvent
deriving instance Eq BankEvent

deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "'" }) ''BankEvent

type BankProjection state = Projection state BankEvent
