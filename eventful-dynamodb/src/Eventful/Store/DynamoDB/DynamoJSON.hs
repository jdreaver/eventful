module Eventful.Store.DynamoDB.DynamoJSON
  ( valueToAttributeValue
  , attributeValueToValue
  ) where

import Control.Lens
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.AWS.DynamoDB hiding (Null)

-- | Note, currently we can't properly disambiguate between an empty Object or
-- an empty Array because of amazonka's 'AttributeValue' lenses. We use a very
-- nasty kludge where we replace empty Objects with a string with a very
-- special value. Follow this issue to see when we can remove the kludge:
-- https://github.com/brendanhay/amazonka/issues/282.
valueToAttributeValue :: Value -> AttributeValue
valueToAttributeValue (String v) = attributeValue & avS ?~ v
valueToAttributeValue (Number v) = attributeValue & avN ?~ T.pack (show v)
valueToAttributeValue (Bool v) = attributeValue & avBOOL ?~ v
valueToAttributeValue (Array vs) = attributeValue & avL .~ fmap valueToAttributeValue (V.toList vs)
valueToAttributeValue (Object v)
  | HM.null v = attributeValue & avS ?~ "__eventful-dynamodb-empty-object__"
  | otherwise = attributeValue & avM .~ fmap valueToAttributeValue v
valueToAttributeValue Null = attributeValue & avNULL ?~ True

attributeValueToValue :: AttributeValue -> Value
attributeValueToValue av
  | Just "__eventful-dynamodb-empty-object__" <- av ^. avS = Object HM.empty
  | Just v <- av ^. avS = String v
  | Just v <- av ^. avN = Number $ read $ T.unpack v
  | Just v <- av ^. avBOOL = Bool v
  | Just _ <- av ^. avNULL = Null
  | v <- av ^. avM, not (HM.null v) = Object $ fmap attributeValueToValue v
  | vs <- av ^. avL = Array $ V.fromList $ fmap attributeValueToValue vs
  | otherwise = Null
