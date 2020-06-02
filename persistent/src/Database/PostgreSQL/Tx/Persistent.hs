{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Database.PostgreSQL.Tx.Persistent where

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Database.PostgreSQL.Tx (UnsafeTx(unsafeIOTx), TxM)
import qualified Database.Persist.Postgresql as Persist

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type PgPersistM = Persist.SqlPersistT TxM

-- | Re-export of 'Persist.get'
get
  :: (Persist.PersistRecordBackend record Persist.SqlBackend)
  => Persist.Key record
  -> PgPersistM (Maybe record)
get = unsafeIOTx . Persist.get

-- | Re-export of 'Persist.getJust'
getJust
  :: ( Persist.PersistStoreRead Persist.SqlBackend
     , Show (Persist.Key record)
     , Persist.PersistRecordBackend record Persist.SqlBackend
     )
  => Persist.Key record -> PgPersistM record
getJust = unsafeIOTx . Persist.getJust
