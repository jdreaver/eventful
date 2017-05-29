module Bank.CLI.Store
  ( runDB
  , cliEventStore
  , cliGloballyOrderedEventStore
  , printJSONPretty
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Database.Persist.Sqlite

import Eventful
import Eventful.Store.Sqlite

import Bank.Models
import Bank.ProcessManagers.TransferManager

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStore :: (MonadIO m) => EventStore BankEvent (SqlPersistT m)
cliEventStore = synchronousEventBusWrapper store handlers
  where
    sqlStore = sqliteEventStore defaultSqlEventStoreConfig
    store = serializedEventStore jsonStringSerializer sqlStore
    handlers =
      [ eventPrinter
      , transferManagerHandler
      ]

cliGloballyOrderedEventStore :: (MonadIO m) => GloballyOrderedEventStore BankEvent (SqlPersistT m)
cliGloballyOrderedEventStore =
  serializedGloballyOrderedEventStore jsonStringSerializer
    (sqlGloballyOrderedEventStore defaultSqlEventStoreConfig)

type BankEventHandler m = EventStore BankEvent m -> UUID -> BankEvent -> m ()

eventPrinter :: (MonadIO m) => BankEventHandler m
eventPrinter _ uuid event = liftIO $ printJSONPretty (uuid, event)

transferManagerHandler :: (Monad m) => BankEventHandler m
transferManagerHandler = processManagerHandler transferManagerRouter

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
