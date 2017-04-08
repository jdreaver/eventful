module Bank.Events.Account
  ( AccountOpened (..)
  , AccountCredited (..)
  , AccountDebited (..)
  ) where

import Data.Aeson.TH

import Bank.Json

data AccountOpened =
  AccountOpened
  { accountOpenedOwner :: String
  , accountOpenedInitialFunding :: Double
  } deriving (Show, Eq)

data AccountCredited =
  AccountCredited
  { accountCreditedAmount :: Double
  , accountCreditedReason :: String
  } deriving (Show, Eq)

data AccountDebited =
  AccountDebited
  { accountDebitedAmount :: Double
  , accountDebitedReason :: String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "accountOpened") ''AccountOpened
deriveJSON (unPrefixLower "accountCredited") ''AccountCredited
deriveJSON (unPrefixLower "accountDebited") ''AccountDebited
