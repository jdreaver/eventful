module Bank.Events.Customer
  ( CustomerCreated (..)
  , CustomerAccountAdded (..)
  , CustomerAccountRemoved (..)
  ) where

import Data.Aeson.TH

import Eventful

import Bank.Json

data CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)

data CustomerAccountAdded =
  CustomerAccountAdded
  { customerAccountAddedAccountId :: UUID
  } deriving (Show, Eq)

data CustomerAccountRemoved =
  CustomerAccountRemoved
  { customerAccountRemovedAccountId :: UUID
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customerCreated") ''CustomerCreated
deriveJSON (unPrefixLower "customerAccountAdded") ''CustomerAccountAdded
deriveJSON (unPrefixLower "customerAccountRemoved") ''CustomerAccountRemoved
