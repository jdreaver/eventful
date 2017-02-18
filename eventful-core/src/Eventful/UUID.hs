{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains orphan 'UUID' instances and a few convenience
-- functions around UUIDs. It would be great if this were its own entirely
-- separate package.

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

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Text.Printf (printf)
import Web.PathPieces

uuidFromText :: Text -> Maybe UUID
uuidFromText = fromText

uuidToText :: UUID -> Text
uuidToText = toText

uuidNextRandom :: IO UUID
uuidNextRandom = UUID4.nextRandom

instance PathPiece UUID where
  fromPathPiece = uuidFromText
  toPathPiece = uuidToText

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
