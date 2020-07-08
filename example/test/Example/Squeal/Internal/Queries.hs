{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
module Example.Squeal.Internal.Queries where

import Data.Int (Int32)
import Database.PostgreSQL.Tx (TxM, tx)
import Database.PostgreSQL.Tx.Squeal
import Example.Squeal.Internal.Schema (Schemas)
import qualified Control.Exception as Exception
import qualified Example.Squeal.Internal.DB as DB

new :: Dependencies -> IO DB.Handle
new deps =
  pure DB.Handle
    { DB.insertThreeMessages = insertThreeMessages deps
    , DB.fetchThreeMessages = fetchThreeMessages deps

    , DB.close = mempty
    }

withHandle :: Dependencies -> (DB.Handle -> IO a) -> IO a
withHandle deps = Exception.bracket (new deps) DB.close

newtype Dependencies = Dependencies { conn :: Connection }

run :: Dependencies -> SquealM Schemas Schemas a -> TxM a
run deps = tx (conn deps)

insertThreeMessages
  :: Dependencies
  -> String -> String -> String -> TxM (Int, Int, Int)
insertThreeMessages deps s1 s2 s3 = run deps do
  go >>= \case
    [k1, k2, k3] -> pure (k1, k2, k3)
    rows -> error $ "Expected exactly 3 rows, got " <> show (length rows)
  where
  go :: SquealM Schemas Schemas [Int]
  go = executeParams stmt (s1, s2, s3) >>= getRows

  stmt :: Statement Schemas (String, String, String) Int
  stmt = Manipulation genericParams rowDecoder insertIntoFoo

  rowDecoder = fmap (fromIntegral :: Int32 -> Int) #id

  insertIntoFoo =
    insertInto #foo
      (Values
        ( Default `as` #id :* Set (param @1) `as` #message )
        [ Default `as` #id :* Set (param @2) `as` #message
        , Default `as` #id :* Set (param @3) `as` #message
        ])
      OnConflictDoRaise
      (Returning_ #id)

fetchThreeMessages
  :: Dependencies
  -> Int -> Int -> Int -> TxM (Maybe String, Maybe String, Maybe String)
fetchThreeMessages deps k1 k2 k3 = run deps do
  rows <- go
  pure
    ( lookup k1 rows
    , lookup k2 rows
    , lookup k3 rows
    )
  where
  go :: SquealM Schemas Schemas [(Int, String)]
  go = executeParams stmt params >>= getRows

  params :: (Int32, Int32, Int32)
  params =
    ( fromIntegral k1
    , fromIntegral k2
    , fromIntegral k3
    )

  stmt :: Statement Schemas (Int32, Int32, Int32) ((Int, String))
  stmt = Query paramEncoder rowDecoder selectFromFoo

  paramEncoder
    :: EncodeParams
        Schemas
        '[ 'NotNull 'PGint4, 'NotNull 'PGint4, 'NotNull 'PGint4 ]
        (Int32, Int32, Int32)
  paramEncoder = genericParams

  rowDecoder =
    (,)
      <$> fmap (fromIntegral :: Int32 -> Int) #id
      <*> #message

  selectFromFoo =
    select_
      (#id :* #message)
      (from (table #foo)
        & where_ (
            #id .== (param @1)
              .|| #id .== (param @2)
              .|| #id .== (param @3)))
