{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module BankCommon.ReadModels.CustomerAccounts
  ( CustomerAccounts (..)
  , customerAccountsAccountsById
  , customerAccountsCustomerAccounts
  , customerAccountsCustomerIdsByName
  , getCustomerAccountsFromName
  , customerAccountsProjection
  ) where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

import Eventful

import BankCommon.Models

-- | Groups account info by customer so it's easy to see all of a customer's
-- accounts
data CustomerAccounts
  = CustomerAccounts
  { _customerAccountsAccountsById :: Map UUID Account
  , _customerAccountsCustomerAccounts :: Map UUID [UUID]
  , _customerAccountsCustomerIdsByName :: Map String UUID
    -- NOTE: This assumes all customer names are unique. Obviously not true in
    -- the real world.
  } deriving (Show, Eq)

makeLenses ''CustomerAccounts

getCustomerAccountsFromName :: CustomerAccounts -> String -> [(UUID, Account)]
getCustomerAccountsFromName CustomerAccounts{..} name = fromMaybe [] $ do
  customerId <- Map.lookup name _customerAccountsCustomerIdsByName
  accountIds <- Map.lookup customerId _customerAccountsCustomerAccounts
  let lookupAccount uuid = (uuid,) <$> Map.lookup uuid _customerAccountsAccountsById
  return $ mapMaybe lookupAccount accountIds

handleCustomerAccountsEvent :: CustomerAccounts -> VersionedStreamEvent BankEvent -> CustomerAccounts
handleCustomerAccountsEvent accounts (StreamEvent uuid _ (CustomerCreatedEvent (CustomerCreated name))) =
  accounts
  & customerAccountsCustomerIdsByName %~ Map.insert name uuid
handleCustomerAccountsEvent accounts (StreamEvent uuid _ (AccountOpenedEvent event@(AccountOpened customerId _))) =
  accounts
  & customerAccountsAccountsById %~ Map.insert uuid account
  & customerAccountsCustomerAccounts %~ Map.insertWith (++) customerId [uuid]
  where
    account = projectionEventHandler accountProjection (projectionSeed accountProjection) (AccountOpenedAccountEvent event)
-- Assume it's an account event. If it isn't it won't get handled, no biggy.
handleCustomerAccountsEvent accounts (StreamEvent uuid _ event) =
  accounts
  & customerAccountsAccountsById %~ Map.adjust modifyAccount uuid
  where
    modifyAccount account =
      maybe account (projectionEventHandler accountProjection account) (deserialize accountEventSerializer event)

customerAccountsProjection :: Projection CustomerAccounts (VersionedStreamEvent BankEvent)
customerAccountsProjection =
  Projection
  (CustomerAccounts Map.empty Map.empty Map.empty)
  handleCustomerAccountsEvent
