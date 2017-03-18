module Eventful.Store.DynamoDBSpec (spec) where

import Control.Lens ((<&>))
import Network.AWS
import Network.AWS.DynamoDB
import Test.Hspec

import Eventful.Store.DynamoDB
import Eventful.TestHelpers

makeStore :: IO (DynamoDBEventStore AWS, Env)
makeStore = do
  let
    dynamo = setEndpoint False "localhost" 8000 dynamoDB
  env <- newEnv Discover <&> configure dynamo

  let
    store = dynamoDBEventStore defaultDynamoDBEventStoreConfig
  liftIO $ runResourceT . runAWS env $ do
    -- Delete table
    void $ send $ deleteTable (dynamoDBEventStoreConfigTableName defaultDynamoDBEventStoreConfig)

    -- Recreate table
    initializeDynamoDBEventStore defaultDynamoDBEventStoreConfig (provisionedThroughput 1 1)

  return (store, env)

spec :: Spec
spec = do
  describe "DynamoDB event store" $ do
    eventStoreSpec makeStore (\env action -> runResourceT $ runAWS env action)
