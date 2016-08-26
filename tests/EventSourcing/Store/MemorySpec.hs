module EventSourcing.Store.MemorySpec (spec) where

import TestImport

import Test.Hspec

import EventSourcing.Store.Memory

spec :: Spec
spec = do
  describe "TVar memory event store" $ do
    rawStoreSpec eventStoreMapTVar
    serializedStoreSpec eventStoreMapTVar

  describe "IORef memory event store" $ do
    rawStoreSpec eventStoreMapIORef
    serializedStoreSpec eventStoreMapIORef
