module Cafe.ChefTodoList
  ( chefTodoListMain
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.Logger (runNoLoggingT)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (pack)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Options.Applicative
import System.Console.ANSI (clearScreen, setCursorPosition)

import Eventful
import Eventful.ReadModel.Memory
import Eventful.Store.Sqlite

import Cafe.CLI.Options (parseDatabaseFileOption)
import Cafe.CLI.Transformer
import Cafe.Models.Tab

-- | Create an in-memory read model that polls the SQLite event store and
-- updates the chef's todo list.
chefTodoListMain :: IO ()
chefTodoListMain = do
  dbFilePath <- execParser $ info (helper <*> parseDatabaseFileOption) (fullDesc <> progDesc "Chef Todo List Terminal")
  pool <- runNoLoggingT $ createSqlitePool (pack dbFilePath) 1
  readModel <- memoryReadModel Map.empty handleChefReadModelEvents
  runPollingReadModel readModel cliGloballyOrderedEventStore (`runSqlPool` pool) 1

handleChefReadModelEvents
  :: Map UUID [Maybe Food]
  -> [GloballyOrderedEvent (StoredEvent JSONString)]
  -> IO (Map UUID [Maybe Food])
handleChefReadModelEvents foodMap events = do
  let
    tabEvents = mapMaybe (traverse (traverse (deserialize jsonStringSerializer))) events :: [GloballyOrderedEvent (StoredEvent TabEvent)]
    foodMap' = foldl' handleEventToMap foodMap $ map globallyOrderedEventEvent tabEvents
  unless (null events) $ printFood foodMap'
  return foodMap'

handleEventToMap :: Map UUID [Maybe Food] -> StoredEvent TabEvent -> Map UUID [Maybe Food]
handleEventToMap foodMap (StoredEvent uuid _ (TabClosed _)) = Map.delete uuid foodMap
handleEventToMap foodMap storedEvent =
  let
    uuid = storedEventProjectionId storedEvent
    oldList = Map.findWithDefault [] uuid foodMap
  in Map.insert uuid (handleEventToFood oldList $ storedEventEvent storedEvent) foodMap

handleEventToFood :: [Maybe Food] -> TabEvent -> [Maybe Food]
handleEventToFood oldFood (FoodOrdered newFood) = oldFood ++ map Just newFood
handleEventToFood oldFood (FoodPrepared indexes) = setIndexesToNothing indexes oldFood
handleEventToFood food _ = food

printFood :: Map UUID [Maybe Food] -> IO ()
printFood foodMap = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Chef's Todo List:"

  forM_ (Map.keys foodMap) $ \uuid -> do
    let foodItems = catMaybes $ foodMap Map.! uuid
    unless (null foodItems) $ do
      putStrLn $ "Tab: " ++ show uuid
      forM_ foodItems $ \(Food (MenuItem desc _)) -> putStrLn $ "  - Item: " ++ desc
