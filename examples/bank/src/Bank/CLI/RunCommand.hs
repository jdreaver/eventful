module Bank.CLI.RunCommand
  ( runCLICommand
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Database.Persist.Sqlite

import Eventful
import Eventful.Store.Sqlite

import Bank.Aggregates.Account
import Bank.Aggregates.Customer
import Bank.CLI.Options
import Bank.Commands
import Bank.Events

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (CreateCustomerCLI createCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create customer with UUID: "
  print uuid
  let command = CreateCustomer' createCommand
  result <- runDB pool $
    commandStoredAggregate cliEventStore jsonStringSerializer customerAggregate uuid command
  printJSONPretty result
runCLICommand pool (ViewAccountCLI uuid) = do
  (state, _) <- runDB pool $
    getLatestProjection cliEventStore jsonStringSerializer accountProjection uuid
  printJSONPretty state
runCLICommand pool (OpenAccountCLI openCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let command = OpenAccount' openCommand
  result <- runDB pool $
    commandStoredAggregate cliEventStore jsonStringSerializer accountAggregate uuid command
  printJSONPretty result
runCLICommand pool (TransferToAccountCLI sourceId amount targetId) = do
  putStrLn $ "Starting transfer from acccount " ++ show sourceId ++ " to " ++ show targetId

  -- TODO: Put this in a proper process manager or saga.

  transferId <- uuidNextRandom
  let startCommand = TransferToAccount' $ TransferToAccount transferId sourceId amount targetId
  startResult <- runDB pool $
    commandStoredAggregate cliEventStore jsonStringSerializer accountAggregate sourceId startCommand
  printJSONPretty startResult
  case startResult of
    Left err -> print err
    Right _ -> do
      let acceptCommand = AcceptTransfer' $ AcceptTransfer transferId sourceId amount
      acceptResult <- runDB pool $
        commandStoredAggregate cliEventStore jsonStringSerializer accountAggregate targetId acceptCommand
      printJSONPretty acceptResult
      let
        finalEvent =
          case acceptResult of
            Left err -> AccountTransferRejected' $ AccountTransferRejected transferId (show err)
            Right _ -> AccountTransferCompleted' $ AccountTransferCompleted transferId
      printJSONPretty finalEvent
      void $ runDB pool $ storeEvents cliEventStore AnyVersion sourceId [serialize jsonStringSerializer finalEvent]
      runCLICommand pool (ViewAccountCLI sourceId)
      runCLICommand pool (ViewAccountCLI targetId)

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStore :: (MonadIO m) => EventStore JSONString (SqlPersistT m)
cliEventStore = sqliteEventStore defaultSqlEventStoreConfig

-- cliGloballyOrderedEventStore :: (MonadIO m) => GloballyOrderedEventStore JSONString (SqlPersistT m)
-- cliGloballyOrderedEventStore = sqlGloballyOrderedEventStore defaultSqlEventStoreConfig

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
