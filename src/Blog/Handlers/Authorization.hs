{-# LANGUAGE OverloadedStrings #-}

module Blog.Handlers.Authorization
  ( authResponse
  , AuthData
  ) where

import Blog.Exceptions.Exceptions
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple
import Network.Wai

type AuthData = B.ByteString

authResponse ::
     Maybe AuthData -> (Connection -> IO Response) -> Connection -> IO Response
authResponse auth respond conn = do
  isAdmin <- authorizeAdmin auth conn
  if isAdmin
    then respond conn
    else return notFound

authorizeAdmin :: Maybe AuthData -> Connection -> IO Bool
authorizeAdmin auth conn = maybe (return False) (`checkAdmin` conn) auth

checkAdmin :: B.ByteString -> Connection -> IO Bool
checkAdmin uId conn = do
  adminUser <-
    query conn "SELECT user_is_admin FROM users WHERE \"user_id\" = ?" [uId]
  case adminUser of
    [[True]] -> return True
    _ -> return False
