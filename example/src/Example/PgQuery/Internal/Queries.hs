{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
module Example.PgQuery.Internal.Queries where

import Database.PostgreSQL.Tx (Tx(tx), TxM)
import qualified Control.Exception as Exception
import qualified Database.PostgreSQL.Query as PG.Query
import qualified Database.PostgreSQL.Tx.Query as Tx
import qualified Example.PgQuery.Internal.DB as DB

new :: PG.Query.Connection -> Tx.Logger -> IO DB.Handle
new conn logger =
  Exception.evaluate DB.Handle
    { DB.insertTwoMessages
    , DB.fetchTwoMessages

    , DB.close = mempty
    }
  where
  ?deps = Deps { conn, logger }

with
  :: PG.Query.Connection
  -> Tx.Logger
  -> (DB.Handle -> IO a)
  -> IO a
with conn logger = Exception.bracket (new conn logger) DB.close

data Deps = Deps
  { conn :: PG.Query.Connection
  , logger :: Tx.Logger
  }

run :: (?deps :: Deps) => Tx.PgQueryM a -> TxM a
run = tx (conn, logger)
  where
  Deps { conn, logger } = ?deps

insertTwoMessages
  :: (?deps :: Deps)
  => String -> String -> TxM (Int, Int)
insertTwoMessages s1 s2 = run do
  Tx.pgQuery [PG.Query.sqlExp|
    insert into foo(message) values (#{s1}), (#{s2}) returning id
  |] >>= \case
    [PG.Query.Only k1, PG.Query.Only k2] -> pure (k1, k2)
    rows -> error $ "Expected exactly 2 rows, got " <> show (length rows)

fetchTwoMessages
  :: (?deps :: Deps)
  => Int -> Int -> TxM (Maybe String, Maybe String)
fetchTwoMessages k1 k2 = run do
  rows <- Tx.pgQuery [PG.Query.sqlExp|
    select id, message
    from foo
    where id in #{PG.Query.In [k1, k2]}
  |]
  pure (lookup k1 rows, lookup k2 rows)
