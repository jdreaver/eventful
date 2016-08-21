{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Throwaway program to test event bus, store, etc

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Logger (runStderrLoggingT)
import Data.List (foldl')
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist
import Database.Persist.Sql

import EventSourcing

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createSqlitePool "database.db" 2
  eventStore <- sqliteEventStore pool
  --eventStore <- newMemoryEventStore

  projectionStore <- newMemoryProjectionStore :: IO (MemoryProjectionStore ListProjection)
  bus <- eventBus
  registerHandler eventStore bus (\uuid event -> putStrLn $ "Recieved: " ++ show (uuid, event :: String))
  registerProjection eventStore bus projectionStore AddItem
  putStrLn "Enter events:"
  forever $ do
    line <- getLine
    putStrLn $ "Entered: " ++ line
    --publishEvent bus nil line
    --uuid <- nextRandom
    let uuid = nil
    storeAndPublishEvent eventStore bus uuid line

    threadDelay 100000

    es <- getEvents eventStore uuid
    print es

    p <- getProjection projectionStore uuid
    print p

-- newtype MemoryEventStore e = MemoryEventStore { unMemoryEventStore :: TVar [e] }

-- instance EventStore (MemoryEventStore e) IO e where
--   getUuids _ = return []
--   getEvents (MemoryEventStore tvar) _ = atomically $ readTVar tvar
--   storeEvents (MemoryEventStore tvar) _ events = atomically $ modifyTVar' tvar (++ events)

-- newMemoryEventStore :: IO (MemoryEventStore e)
-- newMemoryEventStore = do
--   tvar <- atomically $ newTVar []
--   return (MemoryEventStore tvar)

newtype MemoryProjectionStore p = MemoryProjectionStore { unMemoryProjectionStore :: TVar p }

instance (Projection p) => ProjectionStore (MemoryProjectionStore p) IO p where
  latestApplied _ = return 0
  getProjection (MemoryProjectionStore tvar) _ = atomically $ readTVar tvar
  applyEvents (MemoryProjectionStore tvar) _ events = atomically $ modifyTVar' tvar (\p -> foldl' apply p events)

newMemoryProjectionStore :: (Projection p) => IO (MemoryProjectionStore p)
newMemoryProjectionStore = do
  tvar <- atomically $ newTVar seed
  return (MemoryProjectionStore tvar)


newtype ListProjection = ListProjection { unListProjection :: [String] }
  deriving (Show)

data ListProjectionEvent = AddItem String

instance Projection ListProjection where
  type Event ListProjection = ListProjectionEvent
  seed = ListProjection []
  apply (ListProjection xs) (AddItem x) = ListProjection (xs ++ [x])
