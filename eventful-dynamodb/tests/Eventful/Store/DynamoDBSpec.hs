{-# LANGUAGE OverloadedStrings #-}

module Eventful.Store.DynamoDBSpec (spec) where

import Control.Lens ((<&>))
import Network.AWS
import Network.AWS.DynamoDB
import Test.Hspec

import Eventful
import Eventful.Store.DynamoDB
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "DynamoDB event store" $ do
    eventStoreSpec dynamoRunner

makeStore :: IO (EventStore CounterEvent AWS, Env)
makeStore = do
  let
    dynamo = setEndpoint False "localhost" 8000 dynamoDB
  env <- newEnv Discover <&> configure dynamo

  let
    store = dynamoDBEventStore defaultDynamoDBEventStoreConfig
    store' = serializedEventStore jsonSerializer store
  liftIO $ runResourceT . runAWS env $ do
    -- Delete and recreate table
    deleteDynamoDBEventStoreTable defaultDynamoDBEventStoreConfig
    initializeDynamoDBEventStore defaultDynamoDBEventStoreConfig (provisionedThroughput 1 1)

  return (store', env)

dynamoRunner :: EventStoreRunner AWS
dynamoRunner = EventStoreRunner $ \action -> do
  (store, env) <- makeStore
  runResourceT $ runAWS env (action store)
