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

valueToAttributeValue :: Value -> AttributeValue
valueToAttributeValue (String v)  = attributeValue & avS ?~ v
valueToAttributeValue (Number v)  = attributeValue & avN ?~ T.pack (show v)
valueToAttributeValue (Bool v)    = attributeValue & avBOOL ?~ v
valueToAttributeValue (Array vs)  = attributeValue & avL .~ fmap valueToAttributeValue (V.toList vs)
valueToAttributeValue (Object v)  = attributeValue & avM .~ fmap valueToAttributeValue v
valueToAttributeValue Null        = attributeValue & avNULL ?~ True

attributeValueToValue :: AttributeValue -> Value
attributeValueToValue av
  | Just v <- av ^. avS = String v
  | Just v <- av ^. avN = Number $ read $ T.unpack v
  | Just v <- av ^. avBOOL = Bool v
  | Just _ <- av ^. avNULL = Null
  -- TODO: Telling the difference between an empty array and an empty object is
  -- not possible right now.
  | v <- av ^. avM, not (HM.null v) = Object $ fmap attributeValueToValue v
  | vs <- av ^. avL = Array $ V.fromList $ fmap attributeValueToValue vs
  | otherwise = Null
