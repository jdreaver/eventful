{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventful.UUID
  ( UUID
  , uuidFromText
  , uuidToText
  , nil
  , uuidNextRandom
  , uuidFromInteger
  ) where


import Data.UUID
import qualified Data.UUID.V4 as UUID4

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Text.Printf (printf)
import Web.HttpApiData

uuidFromText :: Text -> Maybe UUID
uuidFromText = fromText

uuidToText :: UUID -> Text
uuidToText = toText

uuidNextRandom :: IO UUID
uuidNextRandom = UUID4.nextRandom

instance ToJSON UUID where
  toJSON uuid = toJSON (toText uuid)

instance FromJSON UUID where
  parseJSON text = do
    uuid <- parseJSON text
    maybe (fail $ "Error parsing UUID " ++ show uuid) pure (fromText uuid)

instance FromHttpApiData UUID where
  parseUrlPiece = maybe (Left "Can't decode UUID in URL path") Right . fromText
  parseQueryParam = maybe (Left "Can't decode UUID in query param") Right . fromText

-- | Useful for testing
uuidFromInteger :: Integer -> UUID
uuidFromInteger i =
  let rawString = take 32 $ printf "%032x" i
      (p1, rest1) = splitAt 8 rawString
      (p2, rest2) = splitAt 4 rest1
      (p3, rest3) = splitAt 4 rest2
      (p4, p5)    = splitAt 4 rest3
      withHyphens = intercalate "-" [p1, p2, p3, p4, p5]
      mUuid = uuidFromText . pack $ withHyphens
  in fromMaybe (error $ "Failure in uuidFrominteger for: " ++ show i) mUuid
