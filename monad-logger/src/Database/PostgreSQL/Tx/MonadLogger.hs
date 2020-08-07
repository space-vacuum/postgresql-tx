{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.MonadLogger where

import Control.Monad.Logger (LoggingT(LoggingT, runLoggingT), Loc, LogLevel, LogSource, LogStr, mapLoggingT)
import Database.PostgreSQL.Tx (TxM, Tx(TxEnv, tx))
import Database.PostgreSQL.Tx.Unsafe (UnsafeTx(unsafeIOTx), UnsafeUnliftTx(unsafeWithRunInIOTx))

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

instance Tx (LoggingT TxM) where
  type TxEnv (LoggingT TxM) = Logger
  tx = flip runLoggingT

instance (UnsafeTx io t) => UnsafeTx (LoggingT io) (LoggingT t) where
  unsafeIOTx = mapLoggingT unsafeIOTx

instance (UnsafeUnliftTx t) => UnsafeUnliftTx (LoggingT t) where
  unsafeWithRunInIOTx inner =
    LoggingT \logger ->
      unsafeWithRunInIOTx \run ->
        inner (run . flip runLoggingT logger)
