module Cafe.CLI
  ( cliMain
  , printJSONPretty
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (pack)
import Database.Persist.Sqlite
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
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  void $ liftIO $ runSqlPool (runMigrationSilent migrateTabEntity) pool

  runCLI pool (runCLICommand optionsCommand)

runCLICommand :: Command -> CLI ()
runCLICommand OpenTab = do
  (key, uuid) <- runDB openTab
  liftIO $ putStrLn $ "Opened tab. Id: " ++ show (fromSqlKey key) ++ ", UUID: " ++ show uuid
runCLICommand ListMenu = liftIO $ do
  putStrLn "Food:"
  let printPair (i, MenuItem desc price) = putStrLn $ show i ++ ": " ++ desc ++ " ($" ++ show price ++ ")"
  mapM_ printPair (zip [0 :: Int ..] $ map unFood allFood)
  putStrLn "Drinks:"
  mapM_ printPair (zip [0 :: Int ..] $ map unDrink allDrinks)
runCLICommand (ViewTab tabId) = do
  uuid <- fromJustNote "Could not find tab with given id" <$> runDB (getTabUuid tabId)
  latest <- runDB $ getLatestProjection cliEventStore tabProjection uuid
  liftIO $ printJSONPretty latest
runCLICommand (TabCommand tabId command) = do
  uuid <- fromJustNote "Could not find tab with given id" <$> runDB (getTabUuid tabId)
  result <- runDB $ commandStoredAggregate cliEventStore tabAggregate uuid command
  case result of
    Left err -> liftIO . putStrLn $ "Error! " ++ show err
    Right events -> do
      liftIO . putStrLn $ "Events: " ++ show events
      latest <- runDB $ getLatestProjection cliEventStore tabProjection uuid
      liftIO . putStrLn $ "Latest state:"
      liftIO $ printJSONPretty latest

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig { confIndent = Spaces 2 })
