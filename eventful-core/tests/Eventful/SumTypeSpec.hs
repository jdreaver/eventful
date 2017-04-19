module Eventful.SumTypeSpec (spec) where

import Test.Hspec

import Eventful.SumType

data EventA = EventA deriving (Show, Eq)
data EventB = EventB deriving (Show, Eq)
data EventC = EventC deriving (Show, Eq)

mkSumType' "MyEvent" [''EventA, ''EventB, ''EventC]

deriving instance Show MyEvent
deriving instance Eq MyEvent

spec :: Spec
spec = do
  describe "mkSumType" $ do
    it "can create events" $ do
      -- The real utility in this test is the fact that it compiles
      length [EventA' EventA, EventB' EventB, EventC' EventC] `shouldBe` 3
