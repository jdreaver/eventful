module Bank.CLI.RunCommand
  ( runCLICommand
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Database.Persist.Sqlite

import Eventful
import Eventful.Store.Sqlite

import Bank.Aggregates.Account
import Bank.CLI.Options

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (ViewAccountCLI uuid) = do
  (state, _) <- runDB pool $ getLatestProjection cliEventStore jsonStringSerializer accountProjection uuid
  printJSONPretty state
runCLICommand pool (OpenAccountCLI openData) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let command = OpenAccount openData
  result <- runDB pool $ commandStoredAggregate cliEventStore jsonStringSerializer accountAggregate uuid command
  printJSONPretty result

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStore :: (MonadIO m) => EventStore JSONString (SqlPersistT m)
cliEventStore = sqliteEventStore defaultSqlEventStoreConfig

-- cliGloballyOrderedEventStore :: (MonadIO m) => GloballyOrderedEventStore JSONString (SqlPersistT m)
-- cliGloballyOrderedEventStore = sqlGloballyOrderedEventStore defaultSqlEventStoreConfig

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
