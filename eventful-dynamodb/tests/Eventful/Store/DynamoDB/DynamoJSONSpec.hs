{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventful.Store.DynamoDB.DynamoJSONSpec (spec) where

import Data.Aeson
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Eventful.Store.DynamoDB.DynamoJSON

spec :: Spec
spec = do
  describe "Value <-> AttributeValue conversion" $ do
    it "Value -> AttributeValue -> Value round trip is idempotent" $ property $
      \value -> attributeValueToValue (valueToAttributeValue value) == value

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number <$> arbitrary
    string = String <$> arbitrary
    array = Array <$> arbitrary
    object' = Object <$> arbitrary
