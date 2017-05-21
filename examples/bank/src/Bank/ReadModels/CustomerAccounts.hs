module Bank.ReadModels.CustomerAccounts
  ( CustomerAccounts (..)
  , getCustomerAccountsFromName
  , customerAccountsProjection
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

import Eventful

import Bank.Aggregates.Account
import Bank.Events

-- | Groups account info by customer so it's easy to see all of a customer's
-- accounts
data CustomerAccounts
  = CustomerAccounts
  { customerAccountsAccountsById :: Map UUID Account
  , customerAccountsCustomerAccounts :: Map UUID [UUID]
  , customerAccountsCustomerIdsByName :: Map String UUID
    -- NOTE: This assumes all customer names are unique. Obviously not true in
    -- the real world.
  } deriving (Show, Eq)

getCustomerAccountsFromName :: CustomerAccounts -> String -> [(UUID, Account)]
getCustomerAccountsFromName CustomerAccounts{..} name = fromMaybe [] $ do
  customerId <- Map.lookup name customerAccountsCustomerIdsByName
  accountIds <- Map.lookup customerId customerAccountsCustomerAccounts
  let lookupAccount uuid = (uuid,) <$> Map.lookup uuid customerAccountsAccountsById
  return $ mapMaybe lookupAccount accountIds

handleCustomerAccountsEvent :: CustomerAccounts -> ProjectionEvent BankEvent -> CustomerAccounts
handleCustomerAccountsEvent accounts (ProjectionEvent uuid (CustomerCreatedEvent (CustomerCreated name))) =
  accounts
  { customerAccountsCustomerIdsByName = Map.insert name uuid (customerAccountsCustomerIdsByName accounts)
  }
handleCustomerAccountsEvent accounts (ProjectionEvent uuid event@(AccountOpenedEvent (AccountOpened customerId _))) =
  accounts
  { customerAccountsAccountsById = Map.insert uuid account (customerAccountsAccountsById accounts)
  , customerAccountsCustomerAccounts = Map.insertWith (++) customerId [uuid] (customerAccountsCustomerAccounts accounts)
  }
  where
    account = projectionEventHandler accountProjection (projectionSeed accountProjection) event
-- Assume it's an account event. If it isn't it won't get handled, no biggy.
-- TODO: This feels nasty, we just blindly apply an event to an Account even if
-- it isn't an account event.
handleCustomerAccountsEvent accounts (ProjectionEvent uuid event) =
  accounts
  { customerAccountsAccountsById = Map.adjust modifyAccount uuid (customerAccountsAccountsById accounts)
  }
  where
    modifyAccount account = projectionEventHandler accountProjection account event

customerAccountsProjection :: Projection CustomerAccounts (ProjectionEvent BankEvent)
customerAccountsProjection =
  Projection
  (CustomerAccounts Map.empty Map.empty Map.empty)
  handleCustomerAccountsEvent
