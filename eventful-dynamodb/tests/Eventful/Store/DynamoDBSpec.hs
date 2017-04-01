module Eventful.Store.DynamoDBSpec (spec) where

import Control.Lens ((<&>))
import Data.Aeson
import Network.AWS
import Network.AWS.DynamoDB
import Test.Hspec

import Eventful.Store.Class
import Eventful.Store.DynamoDB
import Eventful.TestHelpers

makeStore :: IO (EventStore Value AWS, Env)
makeStore = do
  let
    dynamo = setEndpoint False "localhost" 8000 dynamoDB
  env <- newEnv Discover <&> configure dynamo

  let
    store = dynamoDBEventStore defaultDynamoDBEventStoreConfig
  liftIO $ runResourceT . runAWS env $ do
    -- Delete and recreate table
    deleteDynamoDBEventStoreTable defaultDynamoDBEventStoreConfig
    initializeDynamoDBEventStore defaultDynamoDBEventStoreConfig (provisionedThroughput 1 1)

  return (store, env)

spec :: Spec
spec = do
  describe "DynamoDB event store" $ do
    eventStoreSpec makeStore (\env action -> runResourceT $ runAWS env action)
