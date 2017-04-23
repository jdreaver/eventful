module Bank.Commands.Customer
  ( CreateCustomer (..)
  , CustomerCommandError (..)
  ) where

import Data.Aeson.TH

import Bank.Json

data CreateCustomer =
  CreateCustomer
  { createCustomerData :: String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "createCustomer") ''CreateCustomer

data CustomerCommandError
  = CustomerAlreadyExistsError
  deriving (Show, Eq)

deriveJSON defaultOptions ''CustomerCommandError
