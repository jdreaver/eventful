module Eventful.SerializerSpec (spec) where

import Data.Dynamic
import GHC.Generics
import Test.Hspec

import Eventful.Serializer

spec :: Spec
spec = do
  describe "EventSumType" $ do
    it "can serialize events without the constructor" $ do
      dynTypeRep (eventToDyn $ MyEventsEventA EventA) `shouldBe` typeOf EventA
      dynTypeRep (eventToDyn $ MyEventsEventA EventA) `shouldBe` dynTypeRep (eventToDyn $ AllEventsEventA EventA)

    it "can deserialize events with the constructor" $ do
      eventFromDyn (toDyn EventA) `shouldBe` Just (MyEventsEventA EventA)
      eventFromDyn (toDyn EventB) `shouldBe` Just (AllEventsEventB EventB)

      eventFromDyn (eventToDyn $ MyEventsEventA EventA) `shouldBe` Just (AllEventsEventA EventA)
      eventFromDyn (eventToDyn $ AllEventsEventB EventB) `shouldBe` Just (MyEventsEventB EventB)

data EventA = EventA deriving (Show, Eq)
data EventB = EventB deriving (Show, Eq)
data EventC = EventC deriving (Show, Eq)

data AllEvents
  = AllEventsEventA EventA
  | AllEventsEventB EventB
  | AllEventsEventC EventC
  deriving (Show, Eq, Generic)

instance EventSumType AllEvents

data MyEvents
  = MyEventsEventA EventA
  | MyEventsEventB EventB
  deriving (Show, Eq, Generic)

instance EventSumType MyEvents
