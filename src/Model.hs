{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

class FromRow m => Model m id | m -> id, id -> m where
  create :: m -> Connection -> IO ()
  read :: Connection -> IO [m]
  update :: m -> Connection -> IO ()
  delete :: Model m id => id -> Connection -> IO ()