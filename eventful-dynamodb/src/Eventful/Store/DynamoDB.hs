module Eventful.Store.DynamoDB
  ( DynamoDBEventStore
  , DynamoDBEventStoreT
  , DynamoDBEventStoreConfig (..)
  , defaultDynamoDBEventStoreConfig
  , dynamoDBEventStore
  , initializeDynamoDBEventStore
  , runAWSIO
  ) where

import Eventful

import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.Trans.AWS (AWST, runAWST)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS
import Network.AWS.DynamoDB
import Safe
import System.IO

import Eventful.Store.DynamoDB.DynamoJSON

type DynamoDBEventStore m = EventStore DynamoDBEventStoreConfig DynamoJSON (AWST m)
type DynamoDBEventStoreT m = EventStoreT DynamoDBEventStoreConfig DynamoJSON (AWST m)

data DynamoDBEventStoreConfig =
  DynamoDBEventStoreConfig
  { dynamoDBEventStoreConfigTableName :: Text
  , dynamoDBEventStoreUUIDAttributeName :: Text
  , dynamoDBEventStoreVersionAttributeName :: Text
  , dynamoDBEventStoreEventAttributeName :: Text
  } deriving (Show)

defaultDynamoDBEventStoreConfig :: DynamoDBEventStoreConfig
defaultDynamoDBEventStoreConfig =
  DynamoDBEventStoreConfig
  { dynamoDBEventStoreConfigTableName = "Events"
  , dynamoDBEventStoreUUIDAttributeName = "UUID"
  , dynamoDBEventStoreVersionAttributeName = "Version"
  , dynamoDBEventStoreEventAttributeName = "Event"
  }

dynamoDBEventStore :: (MonadAWS (AWST m)) => DynamoDBEventStoreConfig -> DynamoDBEventStore m
dynamoDBEventStore config =
  let
    -- TODO: Handle getting UUIDs, should probably just get rid of this silly
    -- function
    getAllUuidsRaw _ = return []
    getLatestVersionRaw = latestEventVersion
    getEventsRaw config' uuid = getDynamoEvents config' uuid Nothing
    getEventsFromVersionRaw config' uuid vers = getDynamoEvents config' uuid (Just vers)
    storeEventsRaw' = storeDynamoEvents
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore config EventStoreDefinition{..}

getDynamoEvents :: (MonadAWS (AWST m)) => DynamoDBEventStoreConfig -> UUID -> Maybe EventVersion -> AWST m [StoredEvent DynamoJSON]
getDynamoEvents config@DynamoDBEventStoreConfig{..} uuid mStartingVersion = do
  -- TODO: Need to paginate this
  latestEvents <- fmap (view qrsItems) . send $ queryBase config uuid mStartingVersion
  return $ mapMaybe (decodeDynamoEvent config uuid) latestEvents

decodeDynamoEvent :: DynamoDBEventStoreConfig -> UUID -> HashMap Text AttributeValue -> Maybe (StoredEvent DynamoJSON)
decodeDynamoEvent DynamoDBEventStoreConfig{..} uuid attributeMap = do
  versionValue <- HM.lookup dynamoDBEventStoreVersionAttributeName attributeMap
  versionText <- versionValue ^. avN
  version <- EventVersion <$> readMay (T.unpack versionText)
  eventValue <- HM.lookup dynamoDBEventStoreEventAttributeName attributeMap
  event <- DynamoJSON <$> eventValue ^. avS
  return $ StoredEvent uuid version event

latestEventVersion :: (MonadAWS (AWST m)) => DynamoDBEventStoreConfig -> UUID -> AWST m EventVersion
latestEventVersion config@DynamoDBEventStoreConfig{..} uuid = do
  latestEvents <- fmap (view qrsItems) . send $
    queryBase config uuid Nothing
    & qLimit ?~ 1
    & qScanIndexForward ?~ False
  return $ EventVersion $ fromMaybe (-1) $ do
    -- NB: We are in the Maybe monad here
    attributeMap <- headMay latestEvents
    versionValue <- HM.lookup dynamoDBEventStoreVersionAttributeName attributeMap
    version <- versionValue ^. avN
    readMay $ T.unpack version

-- | Convenience function to create a Query value for a given store config and
-- UUID.
queryBase :: DynamoDBEventStoreConfig -> UUID -> Maybe EventVersion -> Query
queryBase DynamoDBEventStoreConfig{..} uuid mStartingVersion =
  query
  dynamoDBEventStoreConfigTableName
  & qKeyConditionExpression ?~ "#uuid = :uuid" <> versionCaseExpression
  & qExpressionAttributeNames .~
    HM.singleton "#uuid" dynamoDBEventStoreUUIDAttributeName <>
    versionVariableName
  & qExpressionAttributeValues .~
    HM.singleton ":uuid" (attributeValue & avS ?~ uuidToText uuid) <>
    versionAttributeValue
  where
    versionCaseExpression = maybe "" (const " AND #version >= :version") mStartingVersion
    versionVariableName = maybe HM.empty (const $ HM.singleton "#version" dynamoDBEventStoreVersionAttributeName) mStartingVersion
    versionAttributeValue = maybe HM.empty (HM.singleton ":version" . mkVersionValue) mStartingVersion
    mkVersionValue (EventVersion version) = attributeValue & avN ?~ T.pack (show version)

storeDynamoEvents :: (MonadAWS (AWST m)) => DynamoDBEventStoreConfig -> UUID -> [DynamoJSON] -> AWST m ()
storeDynamoEvents config@DynamoDBEventStoreConfig{..} uuid events = do
  latestVersion <- latestEventVersion config uuid

  -- TODO: Use BatchWriteItem
  forM_ (zip events [latestVersion + 1..]) $ \(DynamoJSON event, EventVersion version) ->
    send $
      putItem
      dynamoDBEventStoreConfigTableName
      & piItem .~
        HM.fromList
        [ (dynamoDBEventStoreUUIDAttributeName, attributeValue & avS ?~ uuidToText uuid)
        , (dynamoDBEventStoreVersionAttributeName, attributeValue & avN ?~ T.pack (show version))
        , (dynamoDBEventStoreEventAttributeName, attributeValue & avS ?~ event)
        ]

-- | Helpful function to create the events table. If a table already exists
-- with the same name, then this function just uses that one. Note, there are
-- no magic migrations going on here, trust this function at your own risk.
initializeDynamoDBEventStore
  :: (MonadAWS (AWST m))
  => DynamoDBEventStoreConfig
  -> ProvisionedThroughput
  -> AWST m ()
initializeDynamoDBEventStore DynamoDBEventStoreConfig{..} throughput = do
  tablesResponse <- send $
    listTables
    & ltExclusiveStartTableName ?~ dynamoDBEventStoreConfigTableName
    & ltLimit ?~ 1
  when (null $ tablesResponse ^. ltrsTableNames) $
    void $ send $
      createTable
      dynamoDBEventStoreConfigTableName
      (uuidKey :| [versionKey])
      throughput
      & ctAttributeDefinitions .~ attributeDefs
  where
    uuidKey = keySchemaElement dynamoDBEventStoreUUIDAttributeName Hash
    versionKey = keySchemaElement dynamoDBEventStoreVersionAttributeName Range
    attributeDefs =
      [ attributeDefinition dynamoDBEventStoreUUIDAttributeName S
      , attributeDefinition dynamoDBEventStoreVersionAttributeName N
      ]

-- | Convenience function if you don't really care about your amazonka
-- settings.
runAWSIO :: AWS a -> IO a
runAWSIO action = do
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT . runAWST env $ action
