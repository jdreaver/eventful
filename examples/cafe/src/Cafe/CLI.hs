module Cafe.CLI
  ( cliMain
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sqlite
import Data.Text (pack)
import Safe

import Eventful.Store.Sqlite

import Cafe.CLI.Options
import Cafe.CLI.Transformer
import Cafe.DB
import Cafe.Models.Tab

cliMain :: IO ()
cliMain = do
  Options{..} <- runOptionsParser

  -- Set up DB connection
  pool <- runNoLoggingT $ createSqlitePool (pack optionsDatabaseFile) 1
  initializeSqliteEventStore pool
  void $ liftIO $ runSqlPool (runMigrationSilent migrateTabEntity) pool

  runCLI pool (runCLICommand optionsCommand)

runCLICommand :: Command -> CLI ()
runCLICommand OpenTab = do
  (key, uuid) <- runDB openTab
  liftIO $ putStrLn $ "Opened tab. Id: " ++ show (fromSqlKey key) ++ ", UUID: " ++ show uuid
runCLICommand (TabCommand tabId command) = do
  uuid <- fromJustNote "Could not find tab with given id" <$> runDB (getTabUuid tabId)
  result <- runEventStoreCLI $ commandStoredAggregate tabAggregate uuid command
  case result of
    Left err -> liftIO . putStrLn $ "Error! " ++ show err
    Right events -> do
      liftIO . putStrLn $ "Events: " ++ show events
      latest <- runEventStoreCLI $ getLatestProjection tabProjection uuid
      liftIO . putStrLn $ "Latest state:"
      liftIO $ print latest
