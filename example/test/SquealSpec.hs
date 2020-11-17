{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module SquealSpec where

import Database.PostgreSQL.Tx.Squeal
  ( AccessMode(ReadWrite), DeferrableMode(NotDeferrable), IsolationLevel(RepeatableRead)
  , Manipulation(UnsafeManipulation), NullType(NotNull, Null), PGType(PGtext)
  , SquealTxM'(fromSquealTxM), TransactionMode(TransactionMode), SquealEnv, SquealM
  , manipulateParams_, transactionally, transactionallySerializable, transactionally_
  )
import Example.Squeal (Schemas)
import Test.Hspec (Spec)
import Test.Infra (Backend(..), testBackend)

spec :: Spec
spec = testBackend squeal

squeal :: (SquealEnv r) => Backend "squeal-postgresql" r TransactionMode
squeal = Backend
  { raiseException = raiseException'

  , withTransaction = transactionally_
  , withTransactionMode = transactionally
  , withTransactionSerializable = transactionallySerializable

  , transactionMode'RepeatableRead = TransactionMode RepeatableRead ReadWrite NotDeferrable
  }

raiseException' :: String -> Maybe String -> SquealM ()
raiseException' errMsg errCode = fromSquealTxM do
  manipulateParams_
    @Schemas
    @'[ 'NotNull 'PGtext, 'Null 'PGtext ]
    (UnsafeManipulation "select raise_exception($1, $2)")
    (errMsg, errCode)
