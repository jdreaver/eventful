-- | A wrapped UUID datatype without orphan instances.

module EventSourcing.UUID
  ( UUID
  , fromText
  , nil
  , nextRandom
  ) where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import Data.Aeson (ToJSON (..), FromJSON (..))
import Database.Persist
import Database.Persist.Sqlite
import Data.Text (Text)
import Foreign.Storable (Storable)
import System.Random (Random)
import Web.HttpApiData

-- | Wrapper around UUID from uuid package.
newtype UUID = UUID { unUUID :: UUID.UUID }
  deriving (Eq, Ord, Read, Show, Storable, Random)

fromText :: Text -> Maybe UUID
fromText text = UUID <$> UUID.fromText text

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
