{-# LANGUAGE OverloadedStrings #-}

module DbRequests where

import User
import Author
import Category
import Post
import Draft
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

publishDraft :: Integer -> IO ()
publishDraft dId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "UPDATE posts SET text_content = (SELECT draft_text FROM drafts WHERE draft_id=?)" [dId]
  return ()

updateDraft :: Draft -> IO ()
updateDraft Draft{draftId = dId, Draft.postId = pId, draftText = dText} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE drafts SET post_id=?, draft_text=? WHERE draft_id=?"
           (pId, dText, dId)
  return () --maybe I should do something with execute to remove this "return"

deleteDraft :: Integer -> IO ()
deleteDraft dId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM drafts WHERE draft_id=?" [dId]
  return ()

{-
{draftId, postId :: Integer, draftText :: Text}
-}

{-
CREATE TABLE drafts (
  draft_id SERIAL PRIMARY KEY,
  post_id INTEGER REFERENCES posts,
  draft_text TEXT NOT NULL
);
-}

insertDraft :: Draft -> IO ()
insertDraft Draft{Draft.postId = pId, draftText = dText} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO drafts(post_id, draft_text) values (?,?)"
           (pId, dText)
  return () --maybe I should do something with execute to remove this "return"

updateAuthor :: Author -> IO ()
updateAuthor Author{Author.authorId = aId, Author.userId = uId, Author.desc = aDesc} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE authors SET users_id=?, author_desc=? WHERE author_id=?"
           (uId, aDesc, aId)
  return () --maybe I should do something with execute to remove this "return"

deleteAuthor :: Integer -> IO ()
deleteAuthor aId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM authors WHERE author_id=?" [aId]
  return ()

insertAuthor :: Author -> IO ()
insertAuthor Author{Author.userId = uId, Author.desc = aDesc} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO authors(users_id, author_desc) values (?,?)"
           (uId, aDesc)
  return () --maybe I should do something with execute to remove this "return"

updateCategory :: Category -> IO ()
updateCategory Category{Category.categoryId = cId, Category.name = n, Category.parentId = pId} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE categories SET category_name=?, category_parent=? WHERE category_id=?"
           (n, pId, cId)
  return () --maybe I should do something with execute to remove this "return"

deleteCategory :: Integer -> IO ()
deleteCategory cId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM categories WHERE category_id=?" [cId]
  return ()

insertCategory :: Category -> IO ()
insertCategory Category{Category.name = n, Category.parentId = pId} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO categories(category_name, category_parent) values (?,?)"
           (n, pId)
  return () --maybe I should do something with execute to remove this "return"

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
getDrafts = getRecords "drafts" :: IO [Draft]

getRecords :: FromRow m => Text -> IO [m]
getRecords table = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table