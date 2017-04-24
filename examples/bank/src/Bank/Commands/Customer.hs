module Bank.Commands.Customer
  ( CreateCustomer (..)
  ) where

import Data.Aeson.TH

import Bank.Json

data CreateCustomer =
  CreateCustomer
  { createCustomerData :: String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "createCustomer") ''CreateCustomer
