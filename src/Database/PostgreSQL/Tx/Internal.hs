{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Tx.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Database.PostgreSQL.Tx.Internal
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), mapReaderT)
import GHC.TypeLits (ErrorMessage(Text), TypeError)

-- | The transaction monad. An implementation monad for a specific database
-- library can be converted to 'TxM' via the 'tx' method.
--
-- @since 0.1.0.0
newtype TxM a = UnsafeTxM
  { -- | Convert a 'TxM' action to raw 'IO'. This is provided only to give
    -- adaptor libraries access to the underlying 'IO' that 'TxM' wraps.
    --
    -- @since 0.1.0.0
    unsafeRunTxM :: IO a
  } deriving newtype (Functor, Applicative, Monad)

-- | Run an 'IO' action in 'TxM'. Use this function with care - arbitrary 'IO'
-- should only be run within a transaction when truly necessary.
--
-- @since 0.1.0.0
unsafeRunIOInTxM :: IO a -> TxM a
unsafeRunIOInTxM = UnsafeTxM

-- | The 'TxM' monad discourages performing arbitrary 'IO' within a
-- transaction, so this instance generates a type error when client code tries
-- to call 'liftIO'.
--
-- @since 0.1.0.0
instance
  ( TypeError
      ('Text "MonadIO is banned in TxM; use 'unsafeRunIOInTxM' if you are sure this is safe IO")
  ) => MonadIO TxM
  where
  liftIO = undefined

-- | Type class for converting a specific database library implementation monad
-- to 'TxM', given a runtime environment.
--
-- @since 0.1.0.0
class Tx (f :: * -> *) where
  -- | The runtime environment needed to convert a specific database library
  -- implementation monad to 'TxM'.
  --
  -- @since 0.1.0.0
  type TxEnv f :: *
  -- | Converts a specific database library implementation monad to 'TxM'.
  --
  -- @since 0.1.0.0
  tx :: TxEnv f -> f a -> TxM a

instance Tx (ReaderT r TxM) where
  type TxEnv (ReaderT r TxM) = r
  tx = flip runReaderT

-- | Promote an unsafe @io@ action to a safe @t@ transaction (will be some form
-- of 'TxM').
--
-- @since 0.1.0.0
class (MonadIO io, Monad t) => UnsafeTx (io :: * -> *) (t :: * -> *) | t -> io where
  -- | Converts an @io@ action to a @t@, which will be some form of 'TxM'. Use
  -- this function with care - arbitrary 'IO' should only be run within a
  -- transaction when truly necessary.
  --
  -- This function may be used within 'TxM' or a specific database library
  -- implementation monad from the various @postgresql-tx-*@ packages.
  --
  -- @since 0.1.0.0
  unsafeIOTx :: io a -> t a

instance UnsafeTx IO TxM where
  unsafeIOTx = unsafeRunIOInTxM

instance (UnsafeTx io t) => UnsafeTx (ReaderT r io) (ReaderT r t) where
  unsafeIOTx = mapReaderT unsafeIOTx

-- | A variant of 'liftIO' to promote 'IO' directly to some variant of 'TxM'.
--
-- @since 0.2.0.0
unsafeLiftIOTx :: (UnsafeTx io t, MonadIO io) => IO a -> t a
unsafeLiftIOTx = unsafeIOTx . liftIO

-- | Run a specific database library implementation monad in 'IO', given that
-- monad's runtime environment. Use of this function outside of test suites
-- should be rare.
--
-- @since 0.2.0.0
unsafeRunTxInIO :: (Tx f) => TxEnv f -> f a -> IO a
unsafeRunTxInIO env = unsafeRunTxM . tx env

-- | Promotes an unsafe 'io' function to some 'ReaderT' over 'TxM'.
--
-- @since 0.1.0.0
unsafeReaderIOTx
  :: (UnsafeTx (ReaderT r io) (ReaderT r t))
  => (r -> io a) -> ReaderT r t a
unsafeReaderIOTx = unsafeIOTx . ReaderT

-- | Support running a @t@ action in 'IO' via the provided runner function.
--
-- @since 0.2.0.0
class Monad t => UnsafeUnliftTx (t :: * -> *) where
  -- | Run a @t@ action in 'IO' via the provided runner function. Use this
  -- function with care - arbitrary 'IO' should only be run within a transaction
  -- when truly necessary.
  --
  -- This function may be used within 'TxM' or a specific database library
  -- implementation monad from the various @postgresql-tx-*@ packages.
  --
  -- @since 0.2.0.0
  unsafeWithRunInIOTx :: ((forall a. t a -> IO a) -> IO b) -> t b

instance UnsafeUnliftTx TxM where
  unsafeWithRunInIOTx inner = unsafeRunIOInTxM $ inner unsafeRunTxM

instance (UnsafeUnliftTx t) => UnsafeUnliftTx (ReaderT r t) where
  unsafeWithRunInIOTx inner =
    ReaderT \r ->
      unsafeWithRunInIOTx \run ->
        inner (run . flip runReaderT r)

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
