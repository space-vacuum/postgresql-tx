{-# LANGUAGE BlockArguments #-}
module Database.PostgreSQL.Tx
  ( -- * Introduction
    -- $intro

    -- ** Transaction monad
    TxM

    -- ** Transaction environment
  , TxEnv(lookupTxEnv)
  , TxEnvs
  , askTxEnv

    -- ** Exceptions
  , throwExceptionTx
  , mapExceptionTx
  , TxException(..)
  , TxErrorType(..)
  , shouldRetryTx
  , shouldRetryTx'
  ) where

import Database.PostgreSQL.Tx.Internal

-- $intro
--
-- @postgresql-tx@ provides a transaction monad - 'TxM'. When arbitrary 'IO' is
-- attempted to be performed within 'TxM', a type error is generated. Arbitrary
-- 'IO' can still be performed when neccessary, but users must explicitly
-- opt-in to this via the "Database.PostgreSQL.Tx.Unsafe" module's @unsafe*@
-- functions.
--
-- Note that @posgresql-tx@ has no dependencies on any specific
-- @postgres@-related database libraries. This library defines the 'TxM' monad
-- and the infrastructure necessary to adapt specific database libraries for
-- 'TxM' compatiblity. The idea is that an adaptor library provides a means to
-- convert a specific database library's implementation monad into 'TxM'.
-- Application authors can then freely mix 'TxM' database functions together
-- even if the implementations of these database functions are using different
-- underlying database libraries, e.g. @postgresql-query@ , @squeal-postgresql@,
-- @postgresql-simple@, etc.
