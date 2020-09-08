{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Infra where

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT(LoggingT), runStderrLoggingT)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Tx (TxErrorType(TxDeadlockDetected, TxSerializationFailure), TxException(TxException), TxM)
import Database.PostgreSQL.Tx.HEnv (HEnv)
import Database.PostgreSQL.Tx.Query (Logger)
import Database.PostgreSQL.Tx.Squeal (SquealConnection)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Test.Hspec
import qualified Database.PostgreSQL.Simple as PG.Simple
import qualified Database.PostgreSQL.Tx as Tx
import qualified Database.PostgreSQL.Tx.HEnv as HEnv
import qualified Database.PostgreSQL.Tx.Query as Tx.Query
import qualified Database.PostgreSQL.Tx.Squeal.Compat.Simple as Tx.Squeal.Compat.Simple
import qualified Database.PostgreSQL.Tx.Unsafe as Tx.Unsafe
import qualified Example.PgQuery
import qualified Example.PgSimple
import qualified Example.Squeal

testBackend :: (KnownSymbol backend) => Backend backend AppEnv tm -> Spec
testBackend backend = testBackend' backend (pure ())

testBackend'
  :: forall backend tm. (KnownSymbol backend)
  => Backend backend AppEnv tm -> Spec -> Spec
testBackend' Backend {..} extraTests = do
  describe (symbolVal (Proxy @backend)) do
    generatedTests
    extraTests
  where
  generatedTests = do
    describe "transaction runners" do
      it "withTransaction" do
        demoTest (withTransaction)
      it "withTransactionMode Serializable" do
        demoTest (withTransactionMode transactionMode'Serializable)
    describe "TxException" do
      it "wraps serialization_failure" $ withAppEnv \appEnv -> do
        expectTxError TxSerializationFailure do
          Tx.Unsafe.unsafeRunTxM appEnv do
            raiseException "serialization_failure"
      it "wraps deadlock_detected" $ withAppEnv \appEnv -> do
        expectTxError TxDeadlockDetected do
          Tx.Unsafe.unsafeRunTxM appEnv do
            raiseException "deadlock_detected"

demoTest :: (forall a. AppEnv -> AppM a -> IO a) -> IO ()
demoTest runTransaction = withAppEnv \appEnv -> do
  (ms1, ms2, ms3, ms4, ms5, ms6) <- do
    Example.PgSimple.withHandle \pgSimpleDB -> do
      Example.PgQuery.withHandle \pgQueryDB -> do
        Example.Squeal.withHandle \squealDB -> do
          runTransaction appEnv do
            demo pgSimpleDB pgQueryDB squealDB
  ms1 `shouldBe` Just "pg-simple: hi"
  ms2 `shouldBe` Just "pg-query: sup"
  ms3 `shouldBe` Just "pg-query: wut"
  ms4 `shouldBe` Just "squeal: nuthin"
  ms5 `shouldBe` Just "squeal: ye"
  ms6 `shouldBe` Just "squeal: k bye"

type AppM = TxM AppEnv

type AppEnv =
  HEnv
    '[ PG.Simple.Connection
     , Logger
     , SquealConnection
     ]

demo
  :: Example.PgSimple.Handle AppM
  -> Example.PgQuery.Handle AppM
  -> Example.Squeal.Handle AppM
  -> AppM
      ( Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      , Maybe String
      )
demo pgSimpleDB pgQueryDB squealDB = do
  k1 <- Example.PgSimple.insertMessage pgSimpleDB "pg-simple: hi"
  (k2, k3) <- Example.PgQuery.insertTwoMessages pgQueryDB "pg-query: sup" "pg-query: wut"
  ms2 <- Example.PgSimple.fetchMessage pgSimpleDB k2
  (ms1, ms3) <- Example.PgQuery.fetchTwoMessages pgQueryDB k1 k3
  (k4, k5, k6) <- Example.Squeal.insertThreeMessages squealDB "squeal: nuthin" "squeal: ye" "squeal: k bye"
  (ms4, ms5, ms6) <- Example.Squeal.fetchThreeMessages squealDB k4 k5 k6
  pure (ms1, ms2, ms3, ms4, ms5, ms6)

withAppEnv :: (AppEnv -> IO a) -> IO a
withAppEnv f = do
  conn <- PG.Simple.connectPostgreSQL "dbname=postgresql-tx-example"
  withEnv conn \env -> do
    Tx.Unsafe.unsafeRunTxM env do
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        drop table if exists foo
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        create table foo
          ( id serial primary key
          , message text not null unique
          )
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        drop function if exists raise_exception(text, text);
      |]
      void $ Tx.Query.pgExecute [Tx.Query.sqlExp|
        create function raise_exception(message text, error_code text) returns void as $$
          begin
            raise exception '%', message using errcode = error_code;
          end;
        $$ language plpgsql;
      |]
    f env
  where
  logger = toLogger runStderrLoggingT

  withEnv simpleConn g = do
    Tx.Squeal.Compat.Simple.withSquealConnection simpleConn \squealConn -> do
      g $ HEnv.fromTuple
        ( simpleConn
        , logger
        , squealConn
        )

expectTxError :: (HasCallStack) => TxErrorType -> IO () -> IO ()
expectTxError e io = do
  try io >>= \case
    Right _ -> expectationFailure "No exception was thrown"
    Left TxException { Tx.errorType } -> errorType `shouldBe` e

toLogger :: (LoggingT IO () -> IO ()) -> Logger
toLogger f loc src lvl msg =
  f $ LoggingT \logger -> liftIO $ logger loc src lvl msg

-- | Generalized interface for testing a @postgresql-tx@ backend.
data Backend (backend :: Symbol) r tm = Backend
  { -- | Raises an exception from postgresql with the given @errcode@.
    raiseException :: String -> TxM r ()

    -- | Runs a transaction at the default mode.
  , withTransaction :: forall a. r -> TxM r a -> IO a
    -- | Runs a transaction at the specified mode @tm@.
  , withTransactionMode :: forall a. tm -> r -> TxM r a -> IO a

    -- | A transaction mode of @serializable@.
  , transactionMode'Serializable :: tm
  }
