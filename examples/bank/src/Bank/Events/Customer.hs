module Bank.Events.Customer
  ( CustomerCreated (..)
  , CustomerCreationRejected (..)
  ) where

import Data.Aeson.TH

import Bank.Json

data CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)

data CustomerCreationRejected
  = CustomerCreationRejected
  { customerCreationRejectedReason :: String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customerCreated") ''CustomerCreated
deriveJSON (unPrefixLower "customerCreationRejected") ''CustomerCreationRejected
