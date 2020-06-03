{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
module Example.PgQuery.Internal.Queries where

import Database.PostgreSQL.Tx (Tx(tx), TxM)
import qualified Database.PostgreSQL.Tx.Query as Tx
import qualified Control.Exception as Exception
import qualified Example.PgQuery.Internal.DB as DB

new :: Dependencies -> IO DB.Handle
new deps =
  pure DB.Handle
    { DB.insertTwoMessages = insertTwoMessages deps
    , DB.fetchTwoMessages = fetchTwoMessages deps

    , DB.close = mempty
    }

withHandle :: Dependencies -> (DB.Handle -> IO a) -> IO a
withHandle deps = Exception.bracket (new deps) DB.close

data Dependencies = Dependencies
  { conn :: Tx.Connection
  , logger :: Tx.Logger
  }

run :: Dependencies -> Tx.PgQueryM a -> TxM a
run deps = tx (conn, logger)
  where
  Dependencies { conn, logger } = deps

insertTwoMessages
  :: Dependencies -> String -> String -> TxM (Int, Int)
insertTwoMessages deps s1 s2 = run deps do
  Tx.pgQuery [Tx.sqlExp|
    insert into foo(message) values (#{s1}), (#{s2}) returning id
  |] >>= \case
    [Tx.Only k1, Tx.Only k2] -> pure (k1, k2)
    rows -> error $ "Expected exactly 2 rows, got " <> show (length rows)

fetchTwoMessages
  :: Dependencies -> Int -> Int -> TxM (Maybe String, Maybe String)
fetchTwoMessages deps k1 k2 = run deps do
  rows <- Tx.pgQuery [Tx.sqlExp|
    select id, message
    from foo
    where id in #{Tx.In [k1, k2]}
  |]
  pure (lookup k1 rows, lookup k2 rows)
