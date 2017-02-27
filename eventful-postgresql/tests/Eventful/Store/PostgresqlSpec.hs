module Eventful.Store.PostgresqlSpec (spec) where

import Database.Persist.Postgresql
import Test.Hspec

import Eventful.Store.Postgresql
import Eventful.TestHelpers

makeStore :: (MonadIO m) => m (PostgresqlEventStore m, ConnectionPool)
makeStore = do
  -- TODO: Obviously this is hard-coded, make this use environment variables or
  -- something in the future.
  let connString = "host=192.168.168.168 port=5432 user=postgres dbname=eventful_test password=password"

  pool <- liftIO $ runNoLoggingT (createPostgresqlPool connString 1)
  initializePostgresqlEventStore pool
  return (postgresqlEventStore, pool)

spec :: Spec
spec = do
  describe "Postgresql event store" $ do
    eventStoreSpec makeStore (flip runSqlPool)
    sequencedEventStoreSpec sqlGetGloballyOrderedEvents makeStore (flip runSqlPool)
