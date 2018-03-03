{-# LANGUAGE RecordWildCards #-}
module BankPostgres.CLI
  ( bankCLIMain ,
  ) where

import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import Control.Monad.IO.Class
import Control.Monad
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql

import Eventful.Store.Postgresql

import BankPostgres.CLI.Options
import BankPostgres.CLI.RunCommand

bankCLIMain :: IO ()
bankCLIMain = do
  Options {..} <- runOptionsParser
  pool <- runNoLoggingT $ createPostgresqlPool (pack optionsDatabaseConnectionString) 10
  flip runSqlPool pool $ do
    void $ runMigration migrateSqlEvent
    liftIO $ runCLICommand pool optionsCommand
