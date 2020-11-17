{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module PgQuerySpec where

import Data.Functor (void)
import Database.PostgreSQL.Tx.Query
  ( IsolationLevel(RepeatableRead), ReadWriteMode(ReadWrite), TransactionMode(TransactionMode)
  , PgQueryEnv, PgQueryM, pgExecute, pgWithTransaction, pgWithTransactionMode
  , pgWithTransactionSerializable, sqlExp
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
  , withTransactionSerializable = pgWithTransactionSerializable

  , transactionMode'RepeatableRead = TransactionMode RepeatableRead ReadWrite
  }

raiseException' :: String -> Maybe String -> PgQueryM ()
raiseException' errMsg errCode = do
  void $ pgExecute [sqlExp|
    select raise_exception(#{errMsg}, #{errCode})
  |]
