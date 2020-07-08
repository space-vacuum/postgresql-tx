{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Example.Squeal.Internal.Schema where

import Squeal.PostgreSQL

type Schemas = Public Schema

type Schema =
  '[ "foo" ::: 'Table (FooConstraints :=> FooColumns)
   ]

type FooConstraints =
  '[ "foo_pkey" ::: 'PrimaryKey '["id"]
   , "foo_message_key" ::: 'Unique '["message"]
   ]

type FooColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "message" ::: 'NoDef :=> 'NotNull 'PGtext
   ]
