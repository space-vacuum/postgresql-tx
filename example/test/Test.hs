{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Database.PostgreSQL.LibPQ as LibPQ
import Database.PostgreSQL.Tx (TxM)
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Simple.Internal as PG.Simple.Internal
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Example.PgQuery
import qualified Example.PgSimple
import qualified Example.Squeal

main :: IO ()
main = do
  conn <- initDB
  pqConn <- getLibPQConnection conn
  let logger = toLogger runStderrLoggingT

      -- For this demo we are using postgresql-query for handling transactions;
      -- however, we could easily swap this out with any other postgresql-tx
      -- supported library.
  let runTransaction :: TxM a -> IO a
      runTransaction =
        Tx.Query.pgWithTransactionMode
          Tx.Query.defaultTransactionMode
          (conn, logger)

  let pgSimpleDeps =
        Example.PgSimple.Dependencies
          { Example.PgSimple.conn }
  let pgQueryDeps =
        Example.PgQuery.Dependencies
          { Example.PgQuery.conn
          , Example.PgQuery.logger
          }
  let squealDeps =
        Example.Squeal.Dependencies
          { Example.Squeal.conn = pqConn }

  (ms1, ms2, ms3, ms4, ms5, ms6) <- do
    Example.PgSimple.withHandle pgSimpleDeps \pgSimpleDB -> do
      Example.PgQuery.withHandle pgQueryDeps \pgQueryDB -> do
        Example.Squeal.withHandle squealDeps \squealDB -> do
          runTransaction do
            demo pgSimpleDB pgQueryDB squealDB

  ms1 `shouldBe` Just "pg-simple: hi"
  ms2 `shouldBe` Just "pg-query: sup"
  ms3 `shouldBe` Just "pg-query: wut"
  ms4 `shouldBe` Just "squeal: nuthin"
  ms5 `shouldBe` Just "squeal: ye"
  ms6 `shouldBe` Just "squeal: k bye"
  putStrLn "Success!"
  where
  shouldBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
  x `shouldBe` y =
    when (x /= y) do
      error $ show x <> " /= " <> show y

demo
  :: Example.PgSimple.Handle
  -> Example.PgQuery.Handle
  -> Example.Squeal.Handle
  -> TxM
      ( Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      )
demo pgSimpleDB pgQueryDB squealDB = do
  k1 <- Example.PgSimple.insertMessage pgSimpleDB "pg-simple: hi"
  (k2, k3) <- Example.PgQuery.insertTwoMessages pgQueryDB "pg-query: sup" "pg-query: wut"
  ms2 <- Example.PgSimple.fetchMessage pgSimpleDB k2
  (ms1, ms3) <- Example.PgQuery.fetchTwoMessages pgQueryDB k1 k3
  (k4, k5, k6) <- Example.Squeal.insertThreeMessages squealDB "squeal: nuthin" "squeal: ye" "squeal: k bye"
  (ms4, ms5, ms6) <- Example.Squeal.fetchThreeMessages squealDB k4 k5 k6
  pure (ms1, ms2, ms3, ms4, ms5, ms6)

initDB :: IO PG.Simple.Connection
initDB = do
  conn <- PG.Simple.connectPostgreSQL "dbname=postgresql-tx-example"
  _ <- PG.Simple.execute_ conn "drop table if exists foo"
  _ <- PG.Simple.execute_ conn $
        "create table if not exists foo"
          <> "( id serial primary key"
          <> ", message text not null unique"
          <> ")"
  pure conn

getLibPQConnection :: PG.Simple.Connection -> IO LibPQ.Connection
getLibPQConnection conn = do
  withMVar (PG.Simple.Internal.connectionHandle conn) pure

toLogger :: (LoggingT IO () -> IO ()) -> Tx.Query.Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg
