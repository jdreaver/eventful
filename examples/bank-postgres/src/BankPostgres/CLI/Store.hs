{-# LANGUAGE RecordWildCards #-}
module BankPostgres.CLI.Store where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8           as BSL
import           Database.Persist.Postgresql

import           Eventful
import           Eventful.Store.Postgresql

import           BankCommon.Models
import           BankCommon.ProcessManagers.TransferManager

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStoreReader :: (MonadIO m) => VersionedEventStoreReader (SqlPersistT m) BankEvent
cliEventStoreReader =
  serializedVersionedEventStoreReader jsonStringSerializer $
  sqlEventStoreReader defaultSqlEventStoreConfig

cliEventStoreWriter :: (MonadIO m) => VersionedEventStoreWriter (SqlPersistT m) BankEvent
cliEventStoreWriter = synchronousEventBusWrapper writer handlers
  where
    sqlStore = postgresqlEventStoreWriter defaultSqlEventStoreConfig
    writer = serializedEventStoreWriter jsonStringSerializer sqlStore
    handlers = [eventPrinter, transferManagerHandler]

cliGlobalEventStoreReader :: (MonadIO m) => GlobalEventStoreReader (SqlPersistT m) BankEvent
cliGlobalEventStoreReader =
  serializedGlobalEventStoreReader jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)

type BankEventHandler m = VersionedEventStoreWriter (SqlPersistT m) BankEvent -> UUID -> BankEvent -> SqlPersistT m ()

eventPrinter :: (MonadIO m) => BankEventHandler m
eventPrinter _ uuid event = liftIO $ printJSONPretty (uuid, event)

transferManagerHandler :: (MonadIO m) => BankEventHandler m
transferManagerHandler writer _ _ = do
  let
    projection = processManagerProjection transferProcessManager
    globalProjection = globalStreamProjection projection
  StreamProjection{..} <- getLatestStreamProjection cliGlobalEventStoreReader globalProjection
  applyProcessManagerCommandsAndEvents transferProcessManager writer cliEventStoreReader streamProjectionState

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
