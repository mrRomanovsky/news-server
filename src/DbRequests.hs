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
    , connectPassword = "news-server" 
  }
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )

updateTag :: Tag -> IO ()
updateTag Tag{Tag.tagId = tId, Tag.tagName = tName} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE tags SET tag_name=? WHERE tag_id=?"
           (tName, tId)
  return () --maybe I should do something with execute to remove this "return"

--UPDATE films SET kind = 'Dramatic' WHERE kind = 'Drama';

deleteTag :: Integer -> IO ()
deleteTag tId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM tags WHERE tag_id=?" [tId]
  return ()

insertTag :: Tag -> IO ()
insertTag Tag{Tag.tagName = n} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO tags(tag_name) values (?)"
           [n]
  return () --maybe I should do something with execute to remove this "return"

deleteUser :: Integer -> IO ()
deleteUser uId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM users WHERE users_id=?" [uId]
  return ()

insertUser :: User -> IO ()
insertUser User{User.name = n, User.surname = s, User.avatar = a} = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO users(users_name, users_surname, avatar, is_admin) values (?, ?, ?, FALSE)"
             (n, s, a)
  return () --maybe I should do something with execute to remove this "return"

getUsers = getRecords "users" :: IO [User]
getCategories = getRecords "categories" :: IO [Category]
getPosts = getRecords "posts" :: IO [Post]
getTags = getRecords "tags" :: IO [Tag]
getAuthors = getRecords "authors" :: IO [Author]

getRecords :: FromRow m => Text -> IO [m]
getRecords table = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table