module Eventful.Store.DynamoDB
  ( DynamoDBEventStore
  , DynamoDBEventStoreT
  , DynamoDBEventStoreConfig (..)
  , defaultDynamoDBEventStoreConfig
  , dynamoDBEventStore
  , initializeDynamoDBEventStore
  , deleteDynamoDBEventStoreTable
  , runAWSIO
  ) where

import Eventful

import Control.Exception (throw, toException)
import Control.Lens
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.AWS (runAWST)
import Data.Aeson
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.List as CL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS
import Network.AWS.DynamoDB
import Safe
import System.IO

import Eventful.Store.DynamoDB.DynamoJSON

type DynamoDBEventStore serialized m = EventStore (DynamoDBEventStoreConfig serialized) serialized m
type DynamoDBEventStoreT serialized m = EventStoreT (DynamoDBEventStoreConfig serialized) serialized m

data DynamoDBEventStoreConfig serialized =
  DynamoDBEventStoreConfig
  { dynamoDBEventStoreConfigTableName :: Text
  , dynamoDBEventStoreConfigUUIDAttributeName :: Text
  , dynamoDBEventStoreConfigVersionAttributeName :: Text
  , dynamoDBEventStoreConfigEventAttributeName :: Text
  , dynamoDBEventStoreConfigSerializedToValue :: serialized -> AttributeValue
  , dynamoDBEventStoreConfigValueToSerialized :: AttributeValue -> serialized
  }

defaultDynamoDBEventStoreConfig :: DynamoDBEventStoreConfig Value
defaultDynamoDBEventStoreConfig =
  DynamoDBEventStoreConfig
  { dynamoDBEventStoreConfigTableName = "Events"
  , dynamoDBEventStoreConfigUUIDAttributeName = "UUID"
  , dynamoDBEventStoreConfigVersionAttributeName = "Version"
  , dynamoDBEventStoreConfigEventAttributeName = "Event"
  , dynamoDBEventStoreConfigSerializedToValue = valueToAttributeValue
  , dynamoDBEventStoreConfigValueToSerialized = attributeValueToValue
  }

dynamoDBEventStore
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> DynamoDBEventStore serialized m
dynamoDBEventStore config =
  let
    getLatestVersionRaw = latestEventVersion
    getEventsFromVersionRaw = getDynamoEvents
    storeEventsRaw' = storeDynamoEvents
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore config EventStoreDefinition{..}

getDynamoEvents
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> UUID
  -> Maybe EventVersion
  -> m [StoredEvent serialized]
getDynamoEvents config@DynamoDBEventStoreConfig{..} uuid mStartingVersion = do
  latestEvents <-
    paginate (queryBase config uuid mStartingVersion) =$=
    CL.concatMap (view qrsItems) $$
    CL.consume
  return $ mapMaybe (decodeDynamoEvent config uuid) latestEvents

decodeDynamoEvent
  :: DynamoDBEventStoreConfig serialized
  -> UUID
  -> HashMap Text AttributeValue
  -> Maybe (StoredEvent serialized)
decodeDynamoEvent DynamoDBEventStoreConfig{..} uuid attributeMap = do
  versionValue <- HM.lookup dynamoDBEventStoreConfigVersionAttributeName attributeMap
  versionText <- versionValue ^. avN
  version <- EventVersion <$> readMay (T.unpack versionText)
  eventAttributeValue <- HM.lookup dynamoDBEventStoreConfigEventAttributeName attributeMap
  let event = dynamoDBEventStoreConfigValueToSerialized eventAttributeValue
  return $ StoredEvent uuid version event

latestEventVersion
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> UUID
  -> m EventVersion
latestEventVersion config@DynamoDBEventStoreConfig{..} uuid = do
  latestEvents <- fmap (view qrsItems) . send $
    queryBase config uuid Nothing
    & qLimit ?~ 1
    & qScanIndexForward ?~ False
  return $ EventVersion $ fromMaybe (-1) $ do
    -- NB: We are in the Maybe monad here
    attributeMap <- headMay latestEvents
    versionValue <- HM.lookup dynamoDBEventStoreConfigVersionAttributeName attributeMap
    version <- versionValue ^. avN
    readMay $ T.unpack version

-- | Convenience function to create a Query value for a given store config and
-- UUID.
queryBase
  :: DynamoDBEventStoreConfig serialized
  -> UUID
  -> Maybe EventVersion
  -> Query
queryBase DynamoDBEventStoreConfig{..} uuid mStartingVersion =
  query
  dynamoDBEventStoreConfigTableName
  & qKeyConditionExpression ?~ "#uuid = :uuid" <> versionCaseExpression
  & qExpressionAttributeNames .~
    HM.singleton "#uuid" dynamoDBEventStoreConfigUUIDAttributeName <>
    versionVariableName
  & qExpressionAttributeValues .~
    HM.singleton ":uuid" (attributeValue & avS ?~ uuidToText uuid) <>
    versionAttributeValue
  where
    versionCaseExpression = maybe "" (const " AND #version >= :version") mStartingVersion
    versionVariableName = maybe HM.empty (const $ HM.singleton "#version" dynamoDBEventStoreConfigVersionAttributeName) mStartingVersion
    versionAttributeValue = maybe HM.empty (HM.singleton ":version" . mkVersionValue) mStartingVersion
    mkVersionValue (EventVersion version) = attributeValue & avN ?~ T.pack (show version)

storeDynamoEvents
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> UUID
  -> [serialized]
  -> m ()
storeDynamoEvents config@DynamoDBEventStoreConfig{..} uuid events = do
  latestVersion <- latestEventVersion config uuid

  -- TODO: Use BatchWriteItem
  forM_ (zip events [latestVersion + 1..]) $ \(event, EventVersion version) ->
    send $
      putItem
      dynamoDBEventStoreConfigTableName
      & piItem .~
        HM.fromList
        [ (dynamoDBEventStoreConfigUUIDAttributeName, attributeValue & avS ?~ uuidToText uuid)
        , (dynamoDBEventStoreConfigVersionAttributeName, attributeValue & avN ?~ T.pack (show version))
        , (dynamoDBEventStoreConfigEventAttributeName, dynamoDBEventStoreConfigSerializedToValue event)
        ]

-- | Helpful function to create the events table. If a table already exists
-- with the same name, then this function just uses that one. Note, there are
-- no magic migrations going on here, trust this function at your own risk.
initializeDynamoDBEventStore
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> ProvisionedThroughput
  -> m ()
initializeDynamoDBEventStore config@DynamoDBEventStoreConfig{..} throughput = do
  eventTableExists <- getDynamoDBEventStoreTableExistence config
  unless eventTableExists $ do
    void $ send $
      createTable
      dynamoDBEventStoreConfigTableName
      (uuidKey :| [versionKey])
      throughput
      & ctAttributeDefinitions .~ attributeDefs
    void $ await tableExists (describeTable dynamoDBEventStoreConfigTableName)
  where
    uuidKey = keySchemaElement dynamoDBEventStoreConfigUUIDAttributeName Hash
    versionKey = keySchemaElement dynamoDBEventStoreConfigVersionAttributeName Range
    attributeDefs =
      [ attributeDefinition dynamoDBEventStoreConfigUUIDAttributeName S
      , attributeDefinition dynamoDBEventStoreConfigVersionAttributeName N
      ]

-- | Checks if the table for the event store exists.
getDynamoDBEventStoreTableExistence
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> m Bool
getDynamoDBEventStoreTableExistence DynamoDBEventStoreConfig{..} = do
  tablesResponse <- trying _ServiceError $ send $
    describeTable
    dynamoDBEventStoreConfigTableName
  case tablesResponse of
    Right response' -> return $ isJust (response' ^. drsTable)
    Left err ->
      if err ^. serviceCode == ErrorCode "ResourceNotFound"
      then return False
      else throw $ toException (ServiceError err)

-- | Convenience function to drop the event store table. Mainly used for
-- testing this library.
deleteDynamoDBEventStoreTable
  :: (MonadAWS m)
  => DynamoDBEventStoreConfig serialized
  -> m ()
deleteDynamoDBEventStoreTable config@DynamoDBEventStoreConfig{..} = do
  eventTableExists <- getDynamoDBEventStoreTableExistence config
  when eventTableExists $ do
    void $ send $ deleteTable dynamoDBEventStoreConfigTableName
    void $ await tableNotExists (describeTable dynamoDBEventStoreConfigTableName)

-- | Convenience function if you don't really care about your amazonka
-- settings.
runAWSIO :: AWS a -> IO a
runAWSIO action = do
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT . runAWST env $ action
