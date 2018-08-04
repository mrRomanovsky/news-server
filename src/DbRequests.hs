{-# LANGUAGE OverloadedStrings #-}

module DbRequests where

import Network.Wai
import Network.HTTP.Types (status200, status404, hAuthorization, Query)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Applicative
import Data.Text

type AuthData = B.ByteString

authResponse :: Maybe AuthData -> (Connection -> IO Response) -> Connection -> IO Response
authResponse auth respond conn = do
  isAdmin <- authorizeAdmin auth conn
  if isAdmin
     then respond conn
     else return notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

authorizeAdmin :: Maybe AuthData -> Connection -> IO Bool
authorizeAdmin auth conn = 
  maybe (return False) (`checkAdmin` conn) auth

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