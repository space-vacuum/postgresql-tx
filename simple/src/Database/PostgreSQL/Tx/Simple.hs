{-# LANGUAGE BlockArguments #-}
module Database.PostgreSQL.Tx.Simple where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Int (Int64)
import Database.PostgreSQL.Tx (TxM)
import Database.PostgreSQL.Tx.Unsafe (unsafeReaderIOTx, unsafeRunTxM)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple

type PgSimpleM = ReaderT Simple.Connection TxM

withTransaction :: Simple.Connection -> TxM a -> IO a
withTransaction c x = Simple.withTransaction c (unsafeRunTxM x)

withTransactionLevel :: Simple.IsolationLevel -> Simple.Connection -> TxM a -> IO a
withTransactionLevel lvl c x = Simple.withTransactionLevel lvl c (unsafeRunTxM x)

unsafeFromPgSimple1
  :: (Simple.Connection -> a1 -> IO r)
  -> a1 -> PgSimpleM r
unsafeFromPgSimple1 f a1 = unsafeReaderIOTx \c -> f c a1

unsafeFromPgSimple2
  :: (Simple.Connection -> a1 -> a2 -> IO r)
  -> a1 -> a2 -> PgSimpleM r
unsafeFromPgSimple2 f a1 a2 = unsafeReaderIOTx \c -> f c a1 a2

-- | Re-export of 'Simple.query'
query :: (Simple.ToRow q, Simple.FromRow r) => Simple.Query -> q -> PgSimpleM [r]
query = unsafeFromPgSimple2 Simple.query

-- | Re-export of 'Simple.query_'
query_ :: (Simple.FromRow r) => Simple.Query -> PgSimpleM [r]
query_ = unsafeFromPgSimple1 Simple.query_

-- | Re-export of 'Simple.execute'
execute :: (Simple.ToRow q) => Simple.Query -> q -> PgSimpleM Int64
execute = unsafeFromPgSimple2 Simple.execute

-- | Re-export of 'Simple.execute_'
execute_ :: Simple.Query -> PgSimpleM Int64
execute_ = unsafeFromPgSimple1 Simple.execute_
