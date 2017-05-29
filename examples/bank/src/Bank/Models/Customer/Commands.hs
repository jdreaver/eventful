{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Commands
  ( customerCommands
  , CreateCustomer (..)
  ) where

import Data.Aeson.TH
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

deriveJSON (unPrefixLower "createCustomer") ''CreateCustomer
