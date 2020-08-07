{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Query
  ( module Database.PostgreSQL.Tx.Query
  , module Database.PostgreSQL.Tx.Query.Internal.Reexport
  ) where

import Control.Monad.Logger (LoggingT, runLoggingT)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Int (Int64)
import Database.PostgreSQL.Tx (Tx(TxEnv, tx), UnsafeTx(unsafeIOTx), UnsafeUnliftTx(unsafeWithRunInIOTx), TxM, unsafeRunTxM)
import Database.PostgreSQL.Tx.Query.Internal.Reexport
import qualified Database.PostgreSQL.Query as Query
import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple
import qualified Database.PostgreSQL.Tx.MonadLogger

type PgQueryM = PgMonadT (LoggingT TxM)

type Logger = Database.PostgreSQL.Tx.MonadLogger.Logger

pgWithTransaction :: (Simple.Connection, Logger) -> TxM a -> IO a
pgWithTransaction = unsafeRunPgQueryTransaction Query.pgWithTransaction

pgWithTransactionMode :: Simple.TransactionMode -> (Simple.Connection, Logger) -> TxM a -> IO a
pgWithTransactionMode m = unsafeRunPgQueryTransaction (Query.pgWithTransactionMode m)

-- Re-export of 'Query.pgQuery'
pgQuery :: (ToSqlBuilder q, FromRow r) => q -> PgQueryM [r]
pgQuery = unsafeIOTx . Query.pgQuery

-- Re-export of 'Query.pgExecute'
pgExecute :: (ToSqlBuilder q) => q -> PgQueryM Int64
pgExecute = unsafeIOTx . Query.pgExecute

unsafePgQueryIOTx :: PgMonadT (LoggingT IO) a -> PgQueryM a
unsafePgQueryIOTx = unsafeIOTx

unsafeRunPgQueryTransaction
  :: (PgMonadT (LoggingT IO) a -> PgMonadT (LoggingT IO) a)
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
  tx (conn, logger) x = runLoggingT (runPgMonadT conn x) logger

instance (UnsafeTx io t) => UnsafeTx (PgMonadT io) (PgMonadT t) where
  unsafeIOTx (PgMonadT x) = PgMonadT $ unsafeIOTx x

instance (UnsafeUnliftTx t) => UnsafeUnliftTx (PgMonadT t) where
  unsafeWithRunInIOTx inner =
    PgMonadT $ unsafeWithRunInIOTx \run ->
      inner (run . unPgMonadT)
