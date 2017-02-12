module Cafe.ChefTodoList
  ( chefTodoListMain
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import Control.Monad.Logger (runNoLoggingT)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (pack)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Options.Applicative
import Safe (maximumDef)
import System.Console.ANSI (clearScreen, setCursorPosition)

import Eventful
import Eventful.Store.Sqlite

import Cafe.CLI.Options (parseDatabaseFileOption)
import Cafe.Models.Tab

-- | Create an in-memory read model that polls the SQLite event store and
-- updates the chef's todo list.
chefTodoListMain :: IO ()
chefTodoListMain = do
  dbFilePath <- execParser $ info (helper <*> parseDatabaseFileOption) (fullDesc <> progDesc "Chef Todo List Terminal")
  pool <- runNoLoggingT $ createSqlitePool (pack dbFilePath) 1
  readModel <- chefTodoReadModel
  runPollingReadModel readModel sqliteEventStore sqliteGetGloballyOrderedEvents (`runSqlPool` pool) 1

chefTodoReadModel :: IO (ReadModel (TVar ChefTodoModel) JSONString IO)
chefTodoReadModel = do
  tvar <- newTVarIO $ ChefTodoModel (-1) Map.empty
  return $ ReadModel tvar getLatestSequence applyChefReadModelEvents
  where
    getLatestSequence tvar' = _chefTodoModelLatestSequenceNumber <$> readTVarIO tvar'

applyChefReadModelEvents :: TVar ChefTodoModel -> [GloballyOrderedEvent (StoredEvent JSONString)] -> IO ()
applyChefReadModelEvents tvar' events = do
  (ChefTodoModel latestSeq foodMap) <- readTVarIO tvar'
  let
    tabEvents = mapMaybe deserialize events :: [GloballyOrderedEvent (StoredEvent TabEvent)]
    latestSeq' = maximumDef latestSeq (globallyOrderedEventSequenceNumber <$> tabEvents)
    foodMap' = foldl' applyEventToMap foodMap $ map globallyOrderedEventEvent tabEvents

  unless (null events) $ printFood foodMap'
  atomically . writeTVar tvar' $ ChefTodoModel latestSeq' foodMap'

data ChefTodoModel =
  ChefTodoModel
  { _chefTodoModelLatestSequenceNumber :: SequenceNumber
  , _chefTodoModelOutsdandingFood :: Map UUID [Maybe Food]
  } deriving (Show, Eq)

applyEventToMap :: Map UUID [Maybe Food] -> StoredEvent TabEvent -> Map UUID [Maybe Food]
applyEventToMap foodMap (StoredEvent uuid _ (TabClosed _)) = Map.delete uuid foodMap
applyEventToMap foodMap storedEvent =
  let
    uuid = storedEventProjectionId storedEvent
    oldList = Map.findWithDefault [] uuid foodMap
  in Map.insert uuid (applyEventToFood oldList $ storedEventEvent storedEvent) foodMap

applyEventToFood :: [Maybe Food] -> TabEvent -> [Maybe Food]
applyEventToFood oldFood (FoodOrdered newFood) = oldFood ++ map Just newFood
applyEventToFood oldFood (FoodPrepared indexes) = setIndexesToNothing indexes oldFood
applyEventToFood food _ = food

printFood :: Map UUID [Maybe Food] -> IO ()
printFood foodMap = do
  let
    allFoods :: [(UUID, Food)]
    allFoods = concatMap (\(uuid, foods) -> mapMaybe (fmap (uuid,)) foods) $ Map.toList foodMap
  clearScreen
  setCursorPosition 0 0
  forM_ allFoods $ \(uuid, Food (MenuItem desc _)) -> putStrLn $ "Tab: " ++ show uuid ++ ", Item: " ++ desc
