module Database.PostgreSQL.Tx.Query.Internal.Reexport
  ( module X
  ) where

-- Don't re-export names that we define in 'Database.PostgreSQL.Tx.Query'.
import Database.PostgreSQL.Query as X hiding
  ( pgWithTransaction
  , pgWithTransactionMode
  , pgQuery
  , pgExecute
  )
