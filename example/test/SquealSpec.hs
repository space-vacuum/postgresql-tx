{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module SquealSpec where

import Database.PostgreSQL.Tx.Squeal
  ( AccessMode(ReadWrite), DeferrableMode(NotDeferrable), IsolationLevel(Serializable)
  , Manipulation(UnsafeManipulation), NullType(NotNull), PGType(PGtext), SquealTxM'(fromSquealTxM)
  , TransactionMode(TransactionMode), SquealEnv, SquealM, manipulateParams_, transactionally
  , transactionally_
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

  , transactionMode'Serializable = TransactionMode Serializable ReadWrite NotDeferrable
  }

raiseException' :: String -> SquealM ()
raiseException' errCode = fromSquealTxM do
  let errMsg = "Raising exception via squeal-postgresql with error code '" <> errCode <> "'"
  manipulateParams_
    @Schemas
    @'[ 'NotNull 'PGtext, 'NotNull 'PGtext ]
    (UnsafeManipulation "select raise_exception($1, $2)")
    (errMsg, errCode)
