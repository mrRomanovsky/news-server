{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where

import Database.PostgreSQL.Simple
import Data.Aeson
import Database.PostgreSQL.Simple.Types

type Page = Integer --for pagination

{-
-type QueryGet = forall m id. Model m id=> Query -> Connection -> IO [m]-}

class (ToJSON m, FromRow m) => Model m id | m -> id, id -> m where
  create :: m -> Connection -> IO ()
  read :: Maybe Page -> Connection -> IO [m]
  update :: m -> Connection -> IO ()
  delete :: Model m id => id -> Connection -> IO ()

{-
paginate :: Model m id => Query -> Maybe Integer -> QueryGet -> Connection -> IO [m]
-}