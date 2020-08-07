{-# LANGUAGE BlockArguments #-}
module Database.PostgreSQL.Tx
  ( TxM
  , Tx(TxEnv, tx)

  , throwExceptionTx
  , mapExceptionTx
  ) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.PostgreSQL.Tx.Internal

throwExceptionTx :: (MonadIO io, UnsafeTx io t, Exception e) => e -> t a
throwExceptionTx ex = unsafeIOTx $ liftIO $ throwIO ex

mapExceptionTx
  :: (UnsafeUnliftTx t, Exception e, Exception e')
  => (e -> Maybe e')
  -> t a
  -> t a
mapExceptionTx mapper action = do
  unsafeWithRunInIOTx \run -> do
    catch (run action) \ex -> do
      case mapper ex of
        Nothing -> throwIO ex
        Just ex' -> throwIO ex'
