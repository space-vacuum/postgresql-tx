{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module PgQuerySpec where

import Data.Functor (void)
import Database.PostgreSQL.Tx.Query
  ( IsolationLevel(Serializable), ReadWriteMode(ReadWrite), TransactionMode(TransactionMode)
  , PgQueryEnv, PgQueryM, pgExecute, pgWithTransaction, pgWithTransactionMode, sqlExp
  )
import Test.Hspec (Spec)
import Test.Infra (Backend(..), testBackend)

spec :: Spec
spec = testBackend postgresqlQuery

postgresqlQuery :: (PgQueryEnv r) => Backend "postgresql-query" r TransactionMode
postgresqlQuery = Backend
  { raiseException = raiseException'

  , withTransaction = pgWithTransaction
  , withTransactionMode = pgWithTransactionMode

  , transactionMode'Serializable = TransactionMode Serializable ReadWrite
  }

raiseException' :: String -> PgQueryM ()
raiseException' errCode = do
  let errMsg = "Raising exception via postgresql-query with error code '" <> errCode <> "'"
  void $ pgExecute [sqlExp|
    select raise_exception(#{errMsg}, #{errCode})
  |]
