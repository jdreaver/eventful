{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventful.Store.DynamoDB.DynamoJSONSpec (spec) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Eventful.Store.DynamoDB.DynamoJSON

spec :: Spec
spec = do
  describe "Value <-> AttributeValue conversion" $ do
    it "Value -> AttributeValue -> Value round trip is idempotent" $ property $
      \value ->
        let value' = removeEmptyObjects value
        in attributeValueToValue (valueToAttributeValue value') == value'

-- Can't handle empty objects right now
removeEmptyObjects :: Value -> Value
removeEmptyObjects (Object xs) =
  if HM.null xs
  then Array V.empty
  else Object $ removeEmptyObjects <$> xs
removeEmptyObjects (Array xs) = Array $ removeEmptyObjects <$> xs
removeEmptyObjects x = x

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
