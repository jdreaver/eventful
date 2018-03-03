{-# LANGUAGE TemplateHaskell #-}

module BankCommon.Models.Customer.Events
  ( customerEvents
  , CustomerCreated (..)
  , CustomerCreationRejected (..)
  ) where

import Language.Haskell.TH (Name)

import BankCommon.Json

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
