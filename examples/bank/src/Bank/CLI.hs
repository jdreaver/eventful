module Bank.CLI
  ( bankCLIMain
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text (pack)
import Database.Persist.Sqlite

import Eventful.Store.Sqlite

import Bank.CLI.Options
import Bank.CLI.RunCommand

bankCLIMain :: IO ()
bankCLIMain = do
  Options{..} <- runOptionsParser

  -- Set up DB connection
  pool <- runNoLoggingT $ createSqlitePool (pack optionsDatabaseFile) 1
  initializeSqliteEventStore defaultSqlEventStoreConfig pool

  runCLICommand pool optionsCommand
