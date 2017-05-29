{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Events
  ( customerEvents
  , CustomerCreated (..)
  , CustomerCreationRejected (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

customerEvents :: [Name]
customerEvents =
  [ ''CustomerCreated
  , ''CustomerCreationRejected
  ]

data CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)

data CustomerCreationRejected
  = CustomerCreationRejected
  { customerCreationRejectedReason :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''CustomerCreated
deriveJSONUnPrefixLower ''CustomerCreationRejected
