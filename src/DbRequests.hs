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
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
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

getPostsByAuthor :: B.ByteString -> IO [Post]
getPostsByAuthor author = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM posts \
             \WHERE author_id = (SELECT author_id FROM authors \
               \WHERE users_id = (SELECT users_id FROM users \
                 \WHERE users_name = ?))" [author]

getPostsWithSubstrInContent :: B.ByteString -> IO [Post]
getPostsWithSubstrInContent substr = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM posts WHERE text_content LIKE ?"
    ["%" `B.append` substr `B.append` "%"]

getPostsWithSubstrInName :: B.ByteString -> IO [Post]
getPostsWithSubstrInName substr = do
  let txt = decodeUtf8 substr
  print txt
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  query conn "SELECT * FROM posts WHERE post_name LIKE ?"
    ["%" `B.append` substr `B.append` "%"]
--'abc' LIKE 'a%'     true
{-
CREATE TABLE posts (
  post_id serial PRIMARY KEY,
  post_name TEXT NOT NULL,
  creation_time timestamp default current_timestamp,
  author_id integer references authors,
  category_id integer references categories,
  tags text[],
  text_content text NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[],
  post_comments text[]
);
-}

publishDraft :: Integer -> IO ()
publishDraft dId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn
   "UPDATE posts SET (creation_time,\
                     \post_name,\
                     \category_id,\
                     \tags,\
                     \text_content,\
                     \main_photo,\
                     \additional_photos) =\
                 \(SELECT creation_time, post_name, category_id, tags, text_content, main_photo, additional_photos\
                 \   FROM drafts WHERE draft_id=?)\
                 \WHERE post_id = (SELECT post_id FROM drafts WHERE draft_id=?)" (dId, dId)
  return () --OPTIMIZE THIS FUNCTION! NOT EFFICIENT! TWO SAME SELECTS!

updateDraft :: Draft -> IO ()
updateDraft Draft{ draftId = dId, Draft.postId = pId, Draft.authorId = aId, Draft.postName = dName
                 , Draft.creationTime = dTime, Draft.categoryId = cId
                 , Draft.tags = dTags, Draft.textContent = dText
                 , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE drafts SET post_id=?, author_id=?, post_name=?, category_id=?, tags=?, text_content=?, main_photo=?, additional_photos=? WHERE draft_id=?"
           (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos, dId)
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

insertDraft :: Draft -> IO ()
insertDraft Draft{ Draft.postId = pId, Draft.authorId = aId, Draft.postName = dName
                 , Draft.creationTime = dTime, Draft.categoryId = cId
                 , Draft.tags = dTags, Draft.textContent = dText
                 , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO drafts(post_id, author_id, post_name, category_id, tags, text_content, main_photo, additional_photos) values (?, ?, ?, ?, ?, ?, ?, ?)"
           (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos)
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