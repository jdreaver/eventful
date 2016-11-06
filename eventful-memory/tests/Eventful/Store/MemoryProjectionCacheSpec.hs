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
    pid1 = ProjectionId (uuidFromInteger 1) :: ProjectionId Counter
    pid2 = ProjectionId (uuidFromInteger 2) :: ProjectionId Counter
    pid3 = ProjectionId (uuidFromInteger 3) :: ProjectionId Counter

  describe "loadProjectionCached" $ do
    store <- runIO memoryEventStoreTVar

    it "should return seed when there are no events" $ do
      snd <$> loadProjectionCached store pid1 projectionCache >>= (`shouldBe` seed)

    it "should have a projection when an event is inserted" $ do
      void $ storeEvents store pid1 [Added 1]
      void $ assertCache store pid1 projectionCache 1

    it "should properly invalidate when more events are inserted" $ do
      void $ storeEvents store pid2 [Added 2]
      cache' <- assertCache store pid2 projectionCache 2
      void $ storeEvents store pid2 [Added 3, Added 4]
      cache'' <- assertCache store pid2 cache' 9
      void $ storeEvents store pid2 [Added (-1), Added (-2)]
      void $ assertCache store pid2 cache'' 6

    it "should give the same answer when new events aren't inserted" $ do
      void $ storeEvents store pid3 [Added 1, Added 2]
      cache <- assertCache store pid3 projectionCache 3
      cache' <- assertCache store pid3 cache 3
      void $ assertCache store pid3 cache' 3

  describe "TVar memory event store wrapped in projection cache store" $ do
    store <- runIO (memoryEventStoreTVar >>= projectionCacheStore)

    it "should properly cache after loading a projection" $ do
      void $ storeEvents store pid1 [Added 1]
      ProjectionCache cache <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup (unProjectionId pid1) cache >>= deserializeCached) `shouldBe` (Nothing :: Maybe (CachedProjection Counter))
      proj <- getLatestProjection store pid1
      proj `shouldBe` Counter 1
      ProjectionCache cache' <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup (unProjectionId pid1) cache' >>= deserializeCached) `shouldBe` Just (CachedProjection 0 (Counter 1))

      void $ storeEvents store pid1 [Added 2, Added 3]
      proj' <- getLatestProjection store pid1
      proj' `shouldBe` Counter 6
      ProjectionCache cache'' <- readTVarIO $ _projectionCacheStoreCache store
      (Map.lookup (unProjectionId pid1) cache'' >>= deserializeCached) `shouldBe` Just (CachedProjection 2 (Counter 6))

    eventStoreSpec (memoryEventStoreTVar >>= projectionCacheStore)
    sequencedEventStoreSpec (memoryEventStoreTVar >>= projectionCacheStore)

assertCache :: TVar MemoryEventStore -> ProjectionId Counter -> ProjectionCache -> Int -> IO ProjectionCache
assertCache store pid cache val = do
  vers <- getLatestVersion store (unProjectionId pid)
  (ProjectionCache cache', proj) <- loadProjectionCached store pid cache
  proj `shouldBe` Counter val
  (Map.lookup (unProjectionId pid) cache' >>= deserializeCached) `shouldBe` Just (CachedProjection vers (Counter val))
  return (ProjectionCache cache')
