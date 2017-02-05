module Cafe.CLI.Transformer
  ( CLI
  , runCLI
  , runDB
  , runEventStoreCLI
  ) where

import Control.Monad.Reader
import Database.Persist.Sql

import Eventful
import Eventful.Store.Sqlite

type CLI = ReaderT ConnectionPool IO

-- | Main entry point into a CLI program.
runCLI :: ConnectionPool -> CLI a -> IO a
runCLI pool action' = runReaderT action' pool

-- | Run a given database action by using the connection pool from the Reader
-- monad.
runDB :: SqlPersistT IO a -> CLI a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool

-- | Run a given event store action.
runEventStoreCLI :: EventStoreT () JSONString (SqlPersistT IO) a -> CLI a
runEventStoreCLI = runDB . runEventStore sqliteEventStore
