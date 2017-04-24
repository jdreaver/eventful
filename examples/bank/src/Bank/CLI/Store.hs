module Bank.CLI.Store
  ( runDB
  , cliEventStore
  , printJSONPretty
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Database.Persist.Sqlite

import Eventful
import Eventful.Store.Sqlite

import Bank.Events

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStore :: (MonadIO m) => EventStore BankEvent (SqlPersistT m)
cliEventStore = synchronousEventBusWrapper store handlers
  where
    sqlStore = sqliteEventStore defaultSqlEventStoreConfig
    store = serializedEventStore jsonStringSerializer sqlStore
    handlers =
      [ eventPrinter
      ]

eventPrinter :: (MonadIO m) => UUID -> BankEvent -> m ()
eventPrinter uuid event = liftIO $ printJSONPretty (uuid, event)

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
