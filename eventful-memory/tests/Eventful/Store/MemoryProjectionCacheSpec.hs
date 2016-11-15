module Eventful.Store.MemoryProjectionCacheSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Hspec

import Eventful.Projection
import Eventful.Store.Memory
import Eventful.Store.MemoryProjectionCache
import Eventful.TestHelpers
import Eventful.UUID

spec :: Spec
spec = do
  let
    uuid1 = uuidFromInteger 1
    uuid2 = uuidFromInteger 2
    uuid3 = uuidFromInteger 3

  describe "loadProjectionCached" $ do
    store <- runIO memoryEventStoreTVar

    it "should return seed when there are no events" $ do
      snd <$> loadProjectionCached store uuid1 projectionCache >>= (`shouldBe` (seed :: Counter))

    it "should have a projection when an event is inserted" $ do
      void $ storeEvents store uuid1 [Added 1]
      void $ assertCache store uuid1 projectionCache 1

    it "should properly invalidate when more events are inserted" $ do
      void $ storeEvents store uuid2 [Added 2]
      cache' <- assertCache store uuid2 projectionCache 2
      void $ storeEvents store uuid2 [Added 3, Added 4]
      cache'' <- assertCache store uuid2 cache' 9
      void $ storeEvents store uuid2 [Added (-1), Added (-2)]
      void $ assertCache store uuid2 cache'' 6

    it "should give the same answer when new events aren't inserted" $ do
      void $ storeEvents store uuid3 [Added 1, Added 2]
      cache <- assertCache store uuid3 projectionCache 3
      cache' <- assertCache store uuid3 cache 3
      void $ assertCache store uuid3 cache' 3

  describe "TVar memory event store wrapped in projection cache store" $ do
    store <- runIO (memoryEventStoreTVar >>= projectionCacheStore)

    it "should properly cache after loading a projection" $ do
      void $ storeEvents store uuid1 [Added 1]
      ProjectionCache cache <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup uuid1 cache >>= deserializeCached) `shouldBe` (Nothing :: Maybe (CachedProjection Counter))
      proj <- getLatestProjection store uuid1
      proj `shouldBe` Counter 1
      ProjectionCache cache' <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup uuid1 cache' >>= deserializeCached) `shouldBe` Just (CachedProjection 0 (Counter 1))

      void $ storeEvents store uuid1 [Added 2, Added 3]
      proj' <- getLatestProjection store uuid1
      proj' `shouldBe` Counter 6
      ProjectionCache cache'' <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup uuid1 cache'' >>= deserializeCached) `shouldBe` Just (CachedProjection 2 (Counter 6))

    eventStoreSpec (memoryEventStoreTVar >>= projectionCacheStore)
    sequencedEventStoreSpec (memoryEventStoreTVar >>= projectionCacheStore)

assertCache :: TVar MemoryEventStore -> UUID -> ProjectionCache -> Int -> IO ProjectionCache
assertCache store uuid cache val = do
  vers <- getLatestVersion store uuid
  (ProjectionCache cache', proj) <- loadProjectionCached store uuid cache
  proj `shouldBe` Counter val
  (Map.lookup uuid cache' >>= deserializeCached) `shouldBe` Just (CachedProjection vers (Counter val))
  return (ProjectionCache cache')
