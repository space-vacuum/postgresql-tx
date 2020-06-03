module Example.Squeal.Internal.DB where

import Database.PostgreSQL.Tx (TxM)

data Handle = Handle
  { insertThreeMessages
      :: String -> String -> String -> TxM (Int, Int, Int)
  , fetchThreeMessages
      :: Int -> Int -> Int -> TxM (Maybe String, Maybe String, Maybe String)

  , close :: IO ()
  }
