{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.Squeal
  ( module Database.PostgreSQL.Tx.Squeal
  , module Database.PostgreSQL.Tx.Squeal.Internal.Reexport
  ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Tx (Tx(TxEnv, tx), UnsafeTx(unsafeIOTx), TxM, unsafeRunTxM)
import Database.PostgreSQL.Tx.Squeal.Internal.Reexport
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP
import qualified Squeal.PostgreSQL as Squeal

type SquealM db0 db1 = Squeal.PQ db0 db1 TxM

unsafeSquealIOTx :: Squeal.PQ db0 db1 IO a -> SquealM db0 db1 a
unsafeSquealIOTx = unsafeIOTx

unsafeSquealIOTx1
  :: (x1 -> Squeal.PQ db0 db1 IO a)
  -> x1 -> SquealM db0 db1 a
unsafeSquealIOTx1 f x1 = unsafeSquealIOTx $ f x1

unsafeSquealIOTx2
  :: (x1 -> x2 -> Squeal.PQ db0 db1 IO a)
  -> x1 -> x2 -> SquealM db0 db1 a
unsafeSquealIOTx2 f x1 x2 = unsafeSquealIOTx $ f x1 x2

unsafeSquealIOTx3
  :: (x1 -> x2 -> x3 -> Squeal.PQ db0 db1 IO a)
  -> x1 -> x2 -> x3 -> SquealM db0 db1 a
unsafeSquealIOTx3 f x1 x2 x3 = unsafeSquealIOTx $ f x1 x2 x3

getRow :: LibPQ.Row -> Result y -> SquealM db db y
getRow = unsafeSquealIOTx2 Squeal.getRow

firstRow :: Result y -> SquealM db db (Maybe y)
firstRow = unsafeSquealIOTx1 Squeal.firstRow

getRows :: Result y -> SquealM db db [y]
getRows = unsafeSquealIOTx1 Squeal.getRows

nextRow :: LibPQ.Row -> Result y -> LibPQ.Row -> SquealM db db (Maybe (LibPQ.Row, y))
nextRow = unsafeSquealIOTx3 Squeal.nextRow

ntuples :: Result y -> SquealM db db LibPQ.Row
ntuples = unsafeSquealIOTx1 Squeal.ntuples

nfields :: Result y -> SquealM db db LibPQ.Column
nfields = unsafeSquealIOTx1 Squeal.nfields

resultStatus :: Result y -> SquealM db db ExecStatus
resultStatus = unsafeSquealIOTx1 Squeal.resultStatus

okResult :: K LibPQ.Result row -> SquealM db db ()
okResult = unsafeSquealIOTx1 Squeal.okResult

resultErrorMessage :: Result y -> SquealM db db (Maybe ByteString)
resultErrorMessage = unsafeSquealIOTx1 Squeal.resultErrorMessage

resultErrorCode :: Result y -> SquealM db db (Maybe ByteString)
resultErrorCode = unsafeSquealIOTx1 Squeal.resultErrorCode

executeParams :: Squeal.Statement db x y -> x -> SquealM db db (Squeal.Result y)
executeParams = unsafeSquealIOTx2 Squeal.executeParams

executeParams_ :: Squeal.Statement db x () -> x -> SquealM db db ()
executeParams_ = unsafeSquealIOTx2 Squeal.executeParams_

execute :: Squeal.Statement db () y -> SquealM db db (Squeal.Result y)
execute = unsafeSquealIOTx1 Squeal.execute

execute_ :: Squeal.Statement db () () -> SquealM db db ()
execute_ = unsafeSquealIOTx1 Squeal.execute_

executePrepared
  :: (Traversable list)
  => Squeal.Statement db x y -> list x -> SquealM db db (list (Squeal.Result y))
executePrepared = unsafeSquealIOTx2 Squeal.executePrepared

executePrepared_
  :: (Foldable list)
  => Squeal.Statement db x () -> list x -> SquealM db db ()
executePrepared_ = unsafeSquealIOTx2 Squeal.executePrepared_

manipulateParams
  :: ( Squeal.GenericParams db params x xs
     , Squeal.GenericRow row y ys
     )
  => Squeal.Manipulation '[] db params row
  -> x
  -> SquealM db db (Squeal.Result y)
manipulateParams = unsafeSquealIOTx2 Squeal.manipulateParams

manipulateParams_
  :: (Squeal.GenericParams db params x xs)
  => Squeal.Manipulation '[] db params '[] -> x -> SquealM db db ()
manipulateParams_ = unsafeSquealIOTx2 Squeal.manipulateParams_

manipulate
  :: (Squeal.GenericRow row y ys)
  => Squeal.Manipulation '[] db '[] row -> SquealM db db (Squeal.Result y)
manipulate = unsafeSquealIOTx1 Squeal.manipulate

manipulate_
  :: Squeal.Manipulation '[] db '[] '[] -> SquealM db db ()
manipulate_ = unsafeSquealIOTx1 Squeal.manipulate_

runQueryParams
  :: ( Squeal.GenericParams db params x xs
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db params row -> x -> SquealM db db (Squeal.Result y)
runQueryParams = unsafeSquealIOTx2 Squeal.runQueryParams

runQuery
  :: ( SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Query '[] '[] db '[] row -> SquealM db db (Squeal.Result y)
runQuery = unsafeSquealIOTx1 Squeal.runQuery

traversePrepared
  :: ( Squeal.GenericParams db params x xs
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => Squeal.Manipulation '[] db params row
  -> list x
  -> SquealM db db (list (Squeal.Result y))
traversePrepared = unsafeSquealIOTx2 Squeal.traversePrepared

forPrepared
  :: ( Squeal.GenericParams db params x xs
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip Squeal.FromField row ys
     )
  => list x
  -> Squeal.Manipulation '[] db params row
  -> SquealM db db (list (Squeal.Result y))
forPrepared = unsafeSquealIOTx2 Squeal.forPrepared

traversePrepared_
  :: ( Squeal.GenericParams db params x xs
     , Foldable list
     )
  => Squeal.Manipulation '[] db params '[]
  -> list x
  -> SquealM db db ()
traversePrepared_ = unsafeSquealIOTx2 Squeal.traversePrepared_

forPrepared_
  :: ( Squeal.GenericParams db params x xs
     , Foldable list
     )
  => list x
  -> Squeal.Manipulation '[] db params '[]
  -> SquealM db db ()
forPrepared_ = unsafeSquealIOTx2 Squeal.forPrepared_

transactionally :: Squeal.TransactionMode -> Squeal.Connection -> TxM a -> IO a
transactionally m = unsafeRunSquealTransaction (Squeal.transactionally m)

transactionally_ :: Squeal.Connection -> TxM a -> IO a
transactionally_ = unsafeRunSquealTransaction Squeal.transactionally_

transactionallyRetry :: Squeal.TransactionMode -> Squeal.Connection -> TxM a -> IO a
transactionallyRetry m = unsafeRunSquealTransaction (Squeal.transactionallyRetry m)

ephemerally :: Squeal.TransactionMode -> Squeal.Connection -> TxM a -> IO a
ephemerally m = unsafeRunSquealTransaction (Squeal.ephemerally m)

ephemerally_ :: Squeal.Connection -> TxM a -> IO a
ephemerally_ = unsafeRunSquealTransaction Squeal.ephemerally_

unsafeRunSquealTransaction
  :: (Squeal.PQ db0 db1 IO a -> Squeal.PQ db0 db1 IO a)
  -> Squeal.Connection
  -> TxM a
  -> IO a
unsafeRunSquealTransaction f conn x =
  flip Squeal.evalPQ (Squeal.K conn)
    $ f
    $ Squeal.PQ \_ -> Squeal.K <$> unsafeRunTxM x

instance Tx (SquealM db0 db1) where
  type TxEnv (SquealM db0 db1) = Squeal.Connection
  tx conn x = Squeal.evalPQ x (Squeal.K conn)

instance (UnsafeTx io t) => UnsafeTx (Squeal.PQ db0 db1 io) (Squeal.PQ db0 db1 t) where
  unsafeIOTx x = Squeal.PQ \kConn -> unsafeIOTx (Squeal.unPQ x kConn)
