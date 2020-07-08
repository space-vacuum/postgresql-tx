module Example.Squeal
  ( DB.Handle(..)
  , Queries.Dependencies(..)
  , Queries.new
  , Queries.withHandle
  ) where

import qualified Example.Squeal.Internal.DB as DB
import qualified Example.Squeal.Internal.Queries as Queries
