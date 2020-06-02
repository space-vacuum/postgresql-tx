{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Query where

import Control.Monad.Logger (LoggingT, runLoggingT)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Int (Int64)
import Database.PostgreSQL.Tx (Tx(TxEnv, tx), UnsafeTx(unsafeIOTx), TxM, unsafeRunTxM)
import Database.PostgreSQL.Tx.MonadLogger (Logger)
import qualified Database.PostgreSQL.Query as Query
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple

type PgQueryM = Query.PgMonadT (LoggingT TxM)

pgWithTransaction :: (Simple.Connection, Logger) -> TxM a -> IO a
pgWithTransaction = unsafeRunPgQueryTransaction Query.pgWithTransaction

pgWithTransactionMode :: Simple.TransactionMode -> (Simple.Connection, Logger) -> TxM a -> IO a
pgWithTransactionMode m = unsafeRunPgQueryTransaction (Query.pgWithTransactionMode m)

-- Re-export of 'Query.pgQuery'
pgQuery :: (Query.ToSqlBuilder q, Query.FromRow r) => q -> PgQueryM [r]
pgQuery = unsafeIOTx . Query.pgQuery

-- Re-export of 'Query.pgExecute'
pgExecute :: (Query.ToSqlBuilder q) => q -> PgQueryM Int64
pgExecute = unsafeIOTx . Query.pgExecute

unsafePgQueryIOTx :: Query.PgMonadT (LoggingT IO) a -> PgQueryM a
unsafePgQueryIOTx = unsafeIOTx

unsafeRunPgQueryTransaction
  :: (Query.PgMonadT (LoggingT IO) a -> Query.PgMonadT (LoggingT IO) a)
  -> (Simple.Connection, Logger)
  -> TxM a
  -> IO a
unsafeRunPgQueryTransaction f (c, logger) x =
  unsafeRunTxM
    $ tx (c, logger)
    $ unsafePgQueryIOTx
    $ f
    $ lift . lift
    $ unsafeRunTxM x

instance Tx PgQueryM where
  type TxEnv PgQueryM = (Simple.Connection, Logger)
  tx (conn, logger) x = runLoggingT (Query.runPgMonadT conn x) logger

instance (UnsafeTx io t) => UnsafeTx (Query.PgMonadT io) (Query.PgMonadT t) where
  unsafeIOTx (Query.PgMonadT x) = Query.PgMonadT $ unsafeIOTx x
