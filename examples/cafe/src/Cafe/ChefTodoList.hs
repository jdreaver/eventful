module Cafe.ChefTodoList
  ( chefTodoListMain
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
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

import Eventful
import Eventful.Store.Sqlite

import Cafe.CLI (printJSONPretty)
import Cafe.CLI.Options (parseDatabaseFileOption)
import Cafe.Models.Tab

-- | Create an in-memory read model that polls the SQLite event store and
-- updates the chef's todo list.
chefTodoListMain :: IO ()
chefTodoListMain = do
  dbFilePath <- execParser $ info (helper <*> parseDatabaseFileOption) (fullDesc <> progDesc "Chef Todo List Terminal")
  pool <- runNoLoggingT $ createSqlitePool (pack dbFilePath) 1
  let model = ChefTodoModel (-1) Map.empty
  chefTodoListLoop pool model

chefTodoListLoop :: ConnectionPool -> ChefTodoModel -> IO ()
chefTodoListLoop pool (ChefTodoModel latestSeq foodMap) = do
  newEvents <- liftIO . flip runSqlPool pool . runEventStore sqliteEventStore $ getSequencedEvents (latestSeq + 1)
  let
    tabEvents = mapMaybe deserialize newEvents :: [StoredEvent TabEvent]
    latestSeq' = maximumDef latestSeq (storedEventSequenceNumber <$> tabEvents)
    foodMap' = foldl' applyEventToMap foodMap tabEvents

  -- mapM_ printJSONPretty (storedEventEvent <$> tabEvents)
  mapM_ printJSONPretty (Map.toList foodMap')

  threadDelay 1000000  -- 1 second in microseconds
  chefTodoListLoop pool $ ChefTodoModel latestSeq' foodMap'

data ChefTodoModel =
  ChefTodoModel
  { _chefTodoModelLatestSequenceNumber :: SequenceNumber
  , _chefTodoModelOutsdandingFood :: Map UUID [Maybe Food]
  } deriving (Show, Eq)

applyEventToMap :: Map UUID [Maybe Food] -> StoredEvent TabEvent -> Map UUID [Maybe Food]
applyEventToMap foodMap (StoredEvent uuid _ _ (TabClosed _)) = Map.delete uuid foodMap
applyEventToMap foodMap storedEvent =
  let
    uuid = storedEventProjectionId storedEvent
    oldList = Map.findWithDefault [] uuid foodMap
  in Map.insert uuid (applyEventToFood oldList $ storedEventEvent storedEvent) foodMap

applyEventToFood :: [Maybe Food] -> TabEvent -> [Maybe Food]
applyEventToFood oldFood (FoodOrdered newFood) = oldFood ++ map Just newFood
applyEventToFood oldFood (FoodPrepared indexes) = setIndexesToNothing indexes oldFood
applyEventToFood food _ = food
