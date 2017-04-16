module Bank.Events.Customer
  ( CustomerCreated (..)
  ) where

import Data.Aeson.TH

import Bank.Json

data CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)


deriveJSON (unPrefixLower "customerCreated") ''CustomerCreated
