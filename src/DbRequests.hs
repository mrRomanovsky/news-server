{-# LANGUAGE OverloadedStrings #-}

module DbRequests where

import User
import Author
import Category
import Post
import Tag
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Control.Monad
import Control.Applicative
import Data.Text

dbTest :: IO ()
dbTest = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "PUT_YOUR_PASSWORD_HERE" 
  }
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )


getUsers = getRecords "users" :: IO [User]
getCategories = getRecords "categories" :: IO [Category]
getPosts = getRecords "posts" :: IO [Post]
getTags = getRecords "tags" :: IO [Tag]
getAuthors = getRecords "authors" :: IO [Author]
{-
putStrLn "3 + 5"
mapM_ print =<< ( query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int] )
-}

getRecords :: FromRow m => Text -> IO [m]
getRecords table = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table

{-getUsers :: IO [User]
getUsers = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM ?" (Only ("users" :: Text)) :: IO [User]-}
  --query_ conn "SELECT * FROM users" :: IO [User]