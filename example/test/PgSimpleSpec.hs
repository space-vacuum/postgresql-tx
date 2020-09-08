{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module PgSimpleSpec where

import Data.Functor (void)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel(Serializable), ReadWriteMode(ReadWrite), TransactionMode(TransactionMode))
import Database.PostgreSQL.Tx.Simple (PgSimpleEnv, execute)
import Test.Hspec (Spec)
import Test.Infra (Backend(..), testBackend)
import qualified Database.PostgreSQL.Tx.Simple as Tx.Simple

spec :: Spec
spec = testBackend postgresqlSimple

postgresqlSimple :: (PgSimpleEnv r) => Backend "postgresql-simple" r TransactionMode
postgresqlSimple = Backend
  { raiseException = raiseException'

  , withTransaction = Tx.Simple.withTransaction
  , withTransactionMode = Tx.Simple.withTransactionMode

  , transactionMode'Serializable = TransactionMode Serializable ReadWrite
  }

raiseException' :: String -> Tx.Simple.PgSimpleM ()
raiseException' errCode = do
  let errMsg = "Raising exception via postgresql-simple with error code '" <> errCode <> "'"
  void $ execute "select raise_exception(?, ?)" (errMsg, errCode)
