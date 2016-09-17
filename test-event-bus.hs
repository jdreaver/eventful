{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Throwaway program to test event bus, store, etc

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Data.Aeson.TH
import Database.Persist.Sqlite (createSqlitePool)

import Eventful

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createSqlitePool "database.db" 2
  eventStore <- sqliteEventStore pool
  --eventStore <- newMemoryEventStore

  (bus :: EventBus JSONString) <- eventBus
  (projectionReadModel, projectionTVar) <- projectionMemoryReadModel
  registerHandler eventStore bus (\event -> putStrLn $ "Recieved: " ++ show event)
  registerReadModel eventStore bus (projectionReadModel :: MemoryReadModel IO JSONString)
  putStrLn "Enter events:"
  forever $ do
    line <- getLine
    putStrLn $ "Entered: " ++ line
    --publishEvent bus nil line
    --uuid <- nextRandom
    let uuid = nil
    storeAndPublishEvent eventStore bus (ProjectionId uuid) (AddItem line)

    threadDelay 100000

    es <- getEvents eventStore (ProjectionId uuid) :: IO [StoredEvent (Event ListProjection)]
    print es

    es' <- getSequencedEvents eventStore 0 :: IO [StoredEvent JSONString]
    print es'

    (p :: ListProjection) <- lookupProjectionMap uuid <$> readTVarIO projectionTVar
    print p

    p' <- getLatestProjection eventStore (ProjectionId uuid :: ProjectionId ListProjection)
    print p'


newtype ListProjection = ListProjection { unListProjection :: [String] }
  deriving (Show, ToJSON, FromJSON)

instance Projection ListProjection where
  data Event ListProjection
    = AddItem String
    deriving (Show)
  seed = ListProjection []
  apply (ListProjection xs) (AddItem x) = ListProjection (xs ++ [x])

deriveJSON defaultOptions 'AddItem
