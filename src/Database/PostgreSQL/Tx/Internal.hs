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
    module Database.PostgreSQL.Tx.Internal
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), mapReaderT)
import GHC.TypeLits (ErrorMessage(Text), TypeError)

newtype TxM a = UnsafeTxM { unsafeRunTxM :: IO a }
  deriving newtype (Functor, Applicative, Monad)

unsafeRunIOInTxM :: IO a -> TxM a
unsafeRunIOInTxM = UnsafeTxM

instance
  ( TypeError
      ('Text "MonadIO is banned in TxM; use 'unsafeRunIOInTxM' if you are sure this is safe IO")
  ) => MonadIO TxM
  where
  liftIO = undefined

-- | Type class for converting an 'f' to 'TxM' given a 'TxEnv'.
class Tx (f :: * -> *) where
  -- | The runtime environment needed to convert 'f' to 'TxM'.
  type TxEnv f :: *
  -- | Converts an 'f' to a 'TxM'.
  tx :: TxEnv f -> f a -> TxM a

instance Tx (ReaderT r TxM) where
  type TxEnv (ReaderT r TxM) = r
  tx = flip runReaderT

-- | Promote an unsafe 'io' action to a safe 't' transaction (will be some form of 'TxM').
class UnsafeTx (io :: * -> *) (t :: * -> *) | t -> io where
  -- | Converts an 'io' action to a 't', which will be some form of 'TxM'.
  unsafeIOTx :: io a -> t a

instance UnsafeTx IO TxM where
  unsafeIOTx = unsafeRunIOInTxM

instance (UnsafeTx io t) => UnsafeTx (ReaderT r io) (ReaderT r t) where
  unsafeIOTx = mapReaderT unsafeIOTx

unsafeRunTxInIO :: (Tx f) => TxEnv f -> f a -> IO a
unsafeRunTxInIO env = unsafeRunTxM . tx env

-- | Promotes an unsafe 'io' function to some 'ReaderT' over 'TxM'.
unsafeReaderIOTx
  :: (UnsafeTx (ReaderT r io) (ReaderT r t))
  => (r -> io a) -> ReaderT r t a
unsafeReaderIOTx = unsafeIOTx . ReaderT

class Monad t => UnsafeUnliftTx (t :: * -> *) where
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
