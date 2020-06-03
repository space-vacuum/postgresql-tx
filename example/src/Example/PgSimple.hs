module Example.PgSimple
  ( DB.Handle(..)
  , Queries.Dependencies(..)
  , Queries.new
  , Queries.withHandle
  ) where

import qualified Example.PgSimple.Internal.DB as DB
import qualified Example.PgSimple.Internal.Queries as Queries
