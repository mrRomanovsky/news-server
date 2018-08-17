{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Blog.Models.Model where

import Data.Aeson
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

type Page = Integer --for pagination

class (ToJSON m, FromRow m) =>
      Model m id
  | m -> id
  , id -> m
  where
  create :: m -> Connection -> IO ()
  getData :: Maybe Page -> Maybe B.ByteString -> Connection -> IO [m]
  update :: m -> Connection -> IO ()
  delete :: Model m id => id -> Connection -> IO ()
