{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Example.PgSimple.Internal.Queries where

import Database.PostgreSQL.Tx (Tx(tx), TxM)
import qualified Control.Exception as Exception
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx.Simple as Tx
import qualified Example.PgSimple.Internal.DB as DB

new :: PG.Simple.Connection -> IO DB.Handle
new conn =
  Exception.evaluate DB.Handle
    { DB.insertMessage
    , DB.fetchMessage

    , DB.close = mempty
    }
  where
  ?deps = Deps { conn }

with :: PG.Simple.Connection -> (DB.Handle -> IO a) -> IO a
with conn = Exception.bracket (new conn) DB.close

newtype Deps = Deps { conn :: PG.Simple.Connection }

run :: (?deps :: Deps) => Tx.PgSimpleM a -> TxM a
run = tx (conn (?deps))

insertMessage :: (?deps :: Deps) => String -> TxM Int
insertMessage s = run do
  Tx.query
    "insert into foo(message) values (?) returning id"
    (PG.Simple.Only s)
    >>= \case
      [PG.Simple.Only k] -> pure k
      rows -> error $ "Expected exactly 1 row, got " <> show (length rows)

fetchMessage :: (?deps :: Deps) => Int -> TxM (Maybe String)
fetchMessage k = run do
  Tx.query
    "select message from foo where id = ?"
    (PG.Simple.Only k)
    >>= \case
      [] -> pure Nothing
      [PG.Simple.Only s] -> pure $ Just s
      rows -> error $ "Expected 0 or 1 rows, got " <> show (length rows)
