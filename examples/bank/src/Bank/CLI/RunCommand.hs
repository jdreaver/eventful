module Bank.CLI.RunCommand
  ( runCLICommand
  ) where

import Control.Monad (void)
import Database.Persist.Sqlite

import Eventful

import Bank.Aggregates.Account
import Bank.Aggregates.Customer
import Bank.CLI.Options
import Bank.CLI.Store
import Bank.Commands
import Bank.ReadModels.CustomerAccounts

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (CreateCustomerCLI createCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create customer with UUID: "
  print uuid
  let command = CreateCustomer' createCommand
  void $ runDB pool $ commandStoredAggregate cliEventStore customerAggregate uuid command
runCLICommand pool (ViewAccountCLI uuid) = do
  (state, _) <- runDB pool $
    getLatestProjection cliEventStore accountProjection uuid
  printJSONPretty state
runCLICommand pool (ViewCustomerAccountsCLI name) = do
  allEvents <- runDB pool $ getSequencedEvents cliGloballyOrderedEventStore 0
  let
    projectionEvents = globallyOrderedEventToProjectionEvent <$> allEvents
    allCustomerAccounts = latestProjection customerAccountsProjection projectionEvents
    thisCustomerAccounts = getCustomerAccountsFromName allCustomerAccounts name
  case thisCustomerAccounts of
    [] -> putStrLn "No accounts found"
    accounts -> mapM_ printJSONPretty accounts
runCLICommand pool (OpenAccountCLI openCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let command = OpenAccount' openCommand
  void $ runDB pool $ commandStoredAggregate cliEventStore accountAggregate uuid command
runCLICommand pool (TransferToAccountCLI sourceId amount targetId) = do
  putStrLn $ "Starting transfer from acccount " ++ show sourceId ++ " to " ++ show targetId

  transferId <- uuidNextRandom
  let startCommand = TransferToAccount' $ TransferToAccount transferId sourceId amount targetId
  void $ runDB pool $ commandStoredAggregate cliEventStore accountAggregate sourceId startCommand
  runCLICommand pool (ViewAccountCLI sourceId)
  runCLICommand pool (ViewAccountCLI targetId)
