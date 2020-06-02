module Example.PgSimple.Internal.DB where

import Database.PostgreSQL.Tx (TxM)

data Handle = Handle
  { insertMessage :: String -> TxM Int
  , fetchMessage :: Int -> TxM (Maybe String)

  , close :: IO ()
  }
