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

makeStore :: IO (VersionedEventStoreWriter AWS CounterEvent, VersionedEventStoreReader AWS CounterEvent, Env)
makeStore = do
  let
    dynamo = setEndpoint False "localhost" 8000 dynamoDB
  env <- newEnv Discover <&> configure dynamo

  let
    writer = serializedEventStoreWriter jsonSerializer $ dynamoDBEventStoreWriter defaultDynamoDBEventStoreConfig
    reader = serializedVersionedEventStoreReader jsonSerializer $ dynamoDBEventStoreReader defaultDynamoDBEventStoreConfig
  liftIO $ runResourceT . runAWS env $ do
    -- Delete and recreate table
    deleteDynamoDBEventStoreTable defaultDynamoDBEventStoreConfig
    initializeDynamoDBEventStore defaultDynamoDBEventStoreConfig (provisionedThroughput 1 1)

  return (writer, reader, env)

dynamoRunner :: EventStoreRunner AWS
dynamoRunner = EventStoreRunner $ \action -> do
  (writer, reader, env) <- makeStore
  runResourceT $ runAWS env (action writer reader)
