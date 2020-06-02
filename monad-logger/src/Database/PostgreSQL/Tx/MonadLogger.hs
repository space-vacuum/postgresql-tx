{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Tx.MonadLogger where

import Database.PostgreSQL.Tx (TxM, Tx(TxEnv, tx), UnsafeTx(unsafeIOTx))
import Control.Monad.Logger (LoggingT(runLoggingT), mapLoggingT, Loc, LogSource, LogLevel, LogStr)

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

instance Tx (LoggingT TxM) where
  type TxEnv (LoggingT TxM) = Logger
  tx = flip runLoggingT

instance (UnsafeTx io t) => UnsafeTx (LoggingT io) (LoggingT t) where
  unsafeIOTx = mapLoggingT unsafeIOTx
