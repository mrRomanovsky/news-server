{-# LANGUAGE OverloadedStrings #-}

module DbRequests where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Applicative
import Data.Text

--'abc' LIKE 'a%'     true

checkAdmin :: B.ByteString -> Connection -> IO Bool
checkAdmin uId conn = do
  adminUser <- query conn "SELECT is_admin FROM users WHERE users_id = ?"
    [uId]
  case adminUser  of
    [[True]] -> return True
    _      -> return False

getRecords :: FromRow m => Text -> Connection -> IO [m]
getRecords table conn =
  query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table