{-# LANGUAGE TemplateHaskell #-}

module BankCommon.Models.Customer.Commands
  ( customerCommands
  , CreateCustomer (..)
  ) where

import Language.Haskell.TH (Name)

import BankCommon.Json

customerCommands :: [Name]
customerCommands =
  [ ''CreateCustomer
  ]

data CreateCustomer =
  CreateCustomer
  { createCustomerData :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''CreateCustomer
