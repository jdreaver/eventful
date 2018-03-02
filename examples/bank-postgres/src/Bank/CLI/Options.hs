module Bank.CLI.Options
  ( runOptionsParser
  , Options (..)
  , CLICommand (..)
  , parseDatabaseConnectionStringOption
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Options.Applicative

import Eventful

import Bank.Models

runOptionsParser :: IO Options
runOptionsParser = execParser $ info (helper <*> parseOptions) (fullDesc <> progDesc "eventful bank CLI")

data Options
  = Options
  { optionsDatabaseConnectionString :: String
  , optionsCommand :: CLICommand
  } deriving (Show)

data CLICommand
  = CreateCustomerCLI CreateCustomer
  | ViewAccountCLI UUID
  | ViewCustomerAccountsCLI String
  | OpenAccountCLI OpenAccount
  | TransferToAccountCLI UUID Double UUID
  deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options <$>
  parseDatabaseConnectionStringOption <*>
  subparser (
    command "create-customer" (info (helper <*> parseCreateCustomer) (progDesc "Create a customer")) <>
    command "view-account" (info (helper <*> parseViewAccount) (progDesc "View an account")) <>
    command "view-customer-accounts" (info (helper <*> parseViewCustomerAccounts) (progDesc "View all customer accounts")) <>
    command "open-account" (info (helper <*> parseOpenAccount) (progDesc "Open a new account")) <>
    command "transfer" (info (helper <*> parseTransfer) (progDesc "Transfer funds to an account"))
  )

parseDatabaseConnectionStringOption :: Parser String
parseDatabaseConnectionStringOption =
  strOption
    ( metavar "DATABASE_CONNECTION_STRING" <>
      long "database-connection-string" <>
      short 'c' <>
      value "database.db" <>
      help "Connection string for database."
    )

parseCreateCustomer :: Parser CLICommand
parseCreateCustomer =
  CreateCustomerCLI . CreateCustomer <$>
  strOption (
    long "name" <>
    metavar "name" <>
    help "Customer's name"
  )

parseViewAccount :: Parser CLICommand
parseViewAccount =
  ViewAccountCLI <$>
  option parseUUID (
    long "account-id" <>
    metavar "uuid" <>
    help "UUID for account stream"
  )

parseViewCustomerAccounts :: Parser CLICommand
parseViewCustomerAccounts =
  ViewCustomerAccountsCLI <$>
  strOption (
    long "name" <>
    metavar "name" <>
    help "Customer's name"
  )

parseOpenAccount :: Parser CLICommand
parseOpenAccount =
  fmap OpenAccountCLI . OpenAccount <$>
  option parseUUID (
    long "owner-id" <>
    metavar "uuid" <>
    help "UUID for the account owner"
  ) <*>
  option auto (
    long "initial-funds" <>
    metavar "amount" <>
    value 0 <>
    help "Initial funds for account."
  )

parseTransfer :: Parser CLICommand
parseTransfer =
  TransferToAccountCLI <$>
  option parseUUID (
    long "account-id" <>
    metavar "uuid" <>
    help "Source account UUID"
  ) <*>
  option auto (
    long "amount" <>
    metavar "amount" <>
    help "Amount to transfer"
  ) <*>
  option parseUUID (
    long "target-id" <>
    metavar "uuid" <>
    help "Target account UUID"
  )

parseUUID :: ReadM UUID
parseUUID = maybe (readerError "Could not parse UUID") return . uuidFromText =<< (T.pack <$> str)
