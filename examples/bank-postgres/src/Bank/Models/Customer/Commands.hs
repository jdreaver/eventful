{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Commands
  ( customerCommands
  , CreateCustomer (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

customerCommands :: [Name]
customerCommands =
  [ ''CreateCustomer
  ]

data CreateCustomer =
  CreateCustomer
  { createCustomerData :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''CreateCustomer
