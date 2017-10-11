{-# LANGUAGE OverloadedStrings #-}

module Eventful.Store.PostgresqlSpec (spec) where

import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import Database.Persist.Postgresql
import System.Environment (lookupEnv)
import Test.Hspec

import Eventful.Store.Postgresql
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "Postgres event store" $ do
    eventStoreSpec postgresStoreRunner
    globalStreamEventStoreSpec postgresStoreGlobalRunner

makeStore
  :: (MonadIO m)
  => m ( VersionedEventStoreWriter (SqlPersistT m) CounterEvent
       , VersionedEventStoreReader (SqlPersistT m) CounterEvent
       , ConnectionPool)
makeStore = do
  let
    makeConnString host port user pass db = (
          "host=" <> host
      <> " port=" <> port
      <> " user=" <> user
      <> " dbname=" <> db
      <> " password=" <> pass)
    writer = serializedEventStoreWriter jsonStringSerializer $
        postgresqlEventStoreWriter defaultSqlEventStoreConfig
    reader = serializedVersionedEventStoreReader jsonStringSerializer $
        sqlEventStoreReader defaultSqlEventStoreConfig
  connString <- makeConnString
    <$> getEnvDef "POSTGRES_HOST" "localhost"
    <*> getEnvDef "POSTGRES_PORT" "5432"
    <*> getEnvDef "POSTGRES_USER" "postgres"
    <*> getEnvDef "POSTGRES_PASSWORD" "password"
    <*> getEnvDef "POSTGRES_DBNAME" "eventful_test"
  pool <- liftIO $ runNoLoggingT (createPostgresqlPool connString 1)
  initializePostgresqlEventStore pool
  liftIO $ runSqlPool truncateTables pool
  return (writer, reader, pool)

getEnvDef :: (MonadIO m) => String -> ByteString -> m ByteString
getEnvDef name def = liftIO $ maybe def UTF8.fromString <$> lookupEnv name

getTables :: MonadIO m => SqlPersistT m [Text]
getTables = do
    tables <-
      rawSql
      "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE';"
      []
    return $ map unSingle tables

truncateTables :: MonadIO m => SqlPersistT m ()
truncateTables = do
    tables <- getTables
    sqlBackend <- ask
    let
      escapedTables = map (connEscapeName sqlBackend . DBName) tables
      query = "TRUNCATE TABLE " <> intercalate ", " escapedTables <> " RESTART IDENTITY CASCADE"
    rawExecute query []

postgresStoreRunner :: EventStoreRunner (SqlPersistT IO)
postgresStoreRunner = EventStoreRunner $ \action -> do
  (writer, reader, pool) <- makeStore
  runSqlPool (action writer reader) pool

postgresStoreGlobalRunner :: GlobalStreamEventStoreRunner (SqlPersistT IO)
postgresStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (writer, _, pool) <- makeStore
  let
    globalReader = serializedGlobalEventStoreReader jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)
  runSqlPool (action writer globalReader) pool
