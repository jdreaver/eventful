-- | A wrapped UUID datatype without orphan instances.

module EventSourcing.UUID
  ( UUID
  , uuidFromText
  , nil
  , nextRandom
  , uuidFromInteger
  ) where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite
import Foreign.Storable (Storable)
import System.Random (Random)
import Text.Printf (printf)
import Web.HttpApiData

import Debug.Trace

-- | Wrapper around UUID from uuid package.
newtype UUID = UUID { unUUID :: UUID.UUID }
  deriving (Eq, Ord, Read, Show, Storable, Random)

uuidFromText :: Text -> Maybe UUID
uuidFromText text = UUID <$> UUID.fromText text

nil :: UUID
nil = UUID UUID.nil

nextRandom :: IO UUID
nextRandom = UUID <$> UUID4.nextRandom

instance PersistField UUID where
  toPersistValue = PersistText . UUID.toText . unUUID
  fromPersistValue (PersistText t) =
    case UUID.fromText t of
      Just x -> Right (UUID x)
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance ToJSON UUID where
  toJSON (UUID uuid) = toJSON (UUID.toText uuid)

instance FromJSON UUID where
  parseJSON text = do
    uuid <- parseJSON text
    maybe (fail $ "Error parsing UUID " ++ show uuid) (pure . UUID) (UUID.fromText uuid)

instance FromHttpApiData UUID where
  parseUrlPiece = maybe (Left "Can't decode UUID in URL path") (Right . UUID) . UUID.fromText
  parseQueryParam = maybe (Left "Can't decode UUID in query param") (Right . UUID) . UUID.fromText

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
