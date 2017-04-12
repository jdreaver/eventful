module Bank.Aggregates.Customer
  ( Customer (..)
  , CustomerEvent (..)
  , customerEventSerializer
  , CustomerProjection
  , customerProjection
  , CustomerCommand (..)
  , CreateCustomerData (..)
  , AddCustomerAccountData (..)
  , RemoveCustomerAccountData (..)
  , CustomerCommandError (..)
  , CustomerAggregate
  , customerAggregate
  ) where

import Data.Aeson.TH
import Data.List (delete)

import Eventful

import Bank.Events
import Bank.Json

data Customer =
  Customer
  { customerName :: Maybe String
  , customerAccountIds :: [UUID]
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

mkEventSumType' "CustomerEvent"
  [ ''CustomerCreated
  , ''CustomerAccountAdded
  , ''CustomerAccountRemoved
  ]
deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

mkSumTypeSerializer "customerEventSerializer" ''CustomerEvent ''BankEvent

applyCustomerEvent :: Customer -> CustomerEvent -> Customer
applyCustomerEvent customer (CustomerCreated' (CustomerCreated name)) =
  customer { customerName = Just name }
applyCustomerEvent customer (CustomerAccountAdded' (CustomerAccountAdded uuid)) =
  customer { customerAccountIds = accountIds' }
  where
    accountIds' =
      if uuid `elem` customerAccountIds customer
      then customerAccountIds customer
      else uuid : customerAccountIds customer
applyCustomerEvent customer (CustomerAccountRemoved' (CustomerAccountRemoved uuid)) =
  customer { customerAccountIds = accountIds' }
  where
    accountIds' = delete uuid (customerAccountIds customer)

type CustomerProjection = Projection Customer CustomerEvent

customerProjection :: CustomerProjection
customerProjection = Projection (Customer Nothing []) applyCustomerEvent

data CustomerCommand
  = CreateCustomer CreateCustomerData
  | AddCustomerAccount AddCustomerAccountData
  | RemoveCustomerAccount RemoveCustomerAccountData
  deriving (Show, Eq)

data CreateCustomerData =
  CreateCustomerData
  { createCustomerDataName :: String
  } deriving (Show, Eq)

data AddCustomerAccountData =
  AddCustomerAccountData
  { addCustomerAccountDataAccountId :: UUID
  } deriving (Show, Eq)

data RemoveCustomerAccountData =
  RemoveCustomerAccountData
  { removeCustomerAccountDataAccountId :: UUID
  } deriving (Show, Eq)

data CustomerCommandError
  = CustomerAlreadyExistsError
  | CustomerAlreadyHasAccountError UUID
  | CustomerHasNoSuchAccountError UUID

deriveJSON defaultOptions ''CustomerCommandError

applyCustomerCommand :: Customer -> CustomerCommand -> Either CustomerCommandError [CustomerEvent]
applyCustomerCommand customer (CreateCustomer (CreateCustomerData name)) =
  case customerName customer of
    Nothing -> Right [CustomerCreated' $ CustomerCreated name]
    Just _ -> Left CustomerAlreadyExistsError
applyCustomerCommand customer (AddCustomerAccount (AddCustomerAccountData uuid)) =
  if uuid `elem` customerAccountIds customer
  then Left (CustomerAlreadyHasAccountError uuid)
  else Right [CustomerAccountAdded' $ CustomerAccountAdded uuid]
applyCustomerCommand customer (RemoveCustomerAccount (RemoveCustomerAccountData uuid)) =
  if uuid `notElem` customerAccountIds customer
  then Left (CustomerHasNoSuchAccountError uuid)
  else Right [CustomerAccountRemoved' $ CustomerAccountRemoved uuid]

type CustomerAggregate = Aggregate Customer CustomerEvent CustomerCommand CustomerCommandError

customerAggregate :: CustomerAggregate
customerAggregate = Aggregate applyCustomerCommand customerProjection
