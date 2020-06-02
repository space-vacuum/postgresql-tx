{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Database.PostgreSQL.Tx (TxM)
import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Example.PgQuery
import qualified Example.PgSimple

main :: IO ()
main = do
  conn <- initDB
  let logger = toLogger runStderrLoggingT
      runTransaction :: TxM a -> IO a
      runTransaction = Tx.Query.pgWithTransaction (conn, logger)
  (ms1, ms2, ms3) <- do
    Example.PgSimple.with conn \pgSimpleDB -> do
      Example.PgQuery.with conn logger \pgQueryDB -> do
        runTransaction do
          demo pgSimpleDB pgQueryDB
  ms1 `shouldBe` Just "hi"
  ms2 `shouldBe` Just "sup"
  ms3 `shouldBe` Just "wut"
  putStrLn "Success!"
  where
  shouldBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
  x `shouldBe` y =
    when (x /= y) do
      error $ show x <> " /= " <> show y

demo
  :: Example.PgSimple.Handle
  -> Example.PgQuery.Handle
  -> TxM (Maybe String, Maybe String, Maybe String)
demo pgSimpleDB pgQueryDB = do
  k1 <- insertMessage "hi"
  (k2, k3) <- insertTwoMessages "sup" "wut"
  ms2 <- fetchMessage k2
  (ms1, ms3) <- fetchTwoMessages k1 k3
  pure (ms1, ms2, ms3)
  where
  Example.PgSimple.Handle
    { Example.PgSimple.insertMessage
    , Example.PgSimple.fetchMessage
    } = pgSimpleDB

  Example.PgQuery.Handle
    { Example.PgQuery.insertTwoMessages
    , Example.PgQuery.fetchTwoMessages
    } = pgQueryDB

initDB :: IO PG.Simple.Connection
initDB = do
  conn <- PG.Simple.connectPostgreSQL "dbname=foo"
  _ <- PG.Simple.execute_ conn "drop table if exists foo"
  _ <- PG.Simple.execute_ conn $
        "create table if not exists foo"
          <> "( id serial primary key"
          <> ", message text not null unique"
          <> ")"
  pure conn

toLogger :: (LoggingT IO () -> IO ()) -> Tx.Query.Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg
