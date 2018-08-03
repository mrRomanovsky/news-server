{-# LANGUAGE OverloadedStrings #-}

module DbRequests where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Applicative
import Data.Text

--'abc' LIKE 'a%'     true

getRecords :: FromRow m => Text -> Connection -> IO [m]
getRecords table conn =
  query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table