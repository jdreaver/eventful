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
import Bank.Events

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (CreateCustomerCLI createData) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create customer with UUID: "
  print uuid
  let command = CreateCustomer createData
  result <- runDB pool $
    commandStoredAggregate cliEventStore (cliSerializer customerEventSerializer) customerAggregate uuid command
  printJSONPretty (fmap (serialize customerEventSerializer) <$> result)
runCLICommand pool (ViewAccountCLI uuid) = do
  (state, _) <- runDB pool $
    getLatestProjection cliEventStore (cliSerializer accountEventSerializer) accountProjection uuid
  printJSONPretty state
runCLICommand pool (OpenAccountCLI openData) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let command = OpenAccount openData
  result <- runDB pool $
    commandStoredAggregate cliEventStore (cliSerializer accountEventSerializer) accountAggregate uuid command
  printJSONPretty (fmap (serialize accountEventSerializer) <$> result)
runCLICommand pool (TransferToAccountCLI sourceId amount targetId) = do
  putStrLn $ "Starting transfer from acccount " ++ show sourceId ++ " to " ++ show targetId

  -- TODO: Put this in a proper process manager or saga.

  transferId <- uuidNextRandom
  let startCommand = TransferToAccount $ TransferToAccountData transferId amount targetId
  startResult <- runDB pool $
    commandStoredAggregate cliEventStore (cliSerializer accountEventSerializer) accountAggregate sourceId startCommand
  printJSONPretty (fmap (serialize accountEventSerializer) <$> startResult)
  case startResult of
    Left err -> print err
    Right _ -> do
      let acceptCommand = AcceptTransfer (AcceptTransferData sourceId amount)
      acceptResult <- runDB pool $
        commandStoredAggregate cliEventStore (cliSerializer accountEventSerializer) accountAggregate targetId acceptCommand
      printJSONPretty (fmap (serialize accountEventSerializer) <$> acceptResult)
      let
        finalEvent =
          case acceptResult of
            Left err -> AccountTransferRejected' $ AccountTransferRejected transferId (show err)
            Right _ -> AccountTransferCompleted' $ AccountTransferCompleted transferId
      printJSONPretty $ serialize accountEventSerializer finalEvent
      void $ runDB pool $ storeEvents cliEventStore AnyVersion sourceId [serialize (cliSerializer accountEventSerializer) finalEvent]
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

cliSerializer :: Serializer a BankEvent -> Serializer a JSONString
cliSerializer serializer = composeSerializers serializer jsonStringSerializer
