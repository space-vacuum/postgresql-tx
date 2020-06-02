module Example.PgQuery.Internal.DB where

import Database.PostgreSQL.Tx (TxM)

data Handle = Handle
  { insertTwoMessages
      :: String -> String -> TxM (Int, Int)
  , fetchTwoMessages
      :: Int -> Int -> TxM (Maybe String, Maybe String)

  , close :: IO ()
  }
