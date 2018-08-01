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

{-
updatePost :: Post -> IO ()
updatePost Post{draftId = dId, Draft.postId = pId, draftText = dText} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE drafts SET post_id=?, draft_text=? WHERE draft_id=?"
           (pId, dText, dId)
  return () --maybe I should do something with execute to remove this "return"

deletePost :: Integer -> IO ()
deletePost pId = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  execute conn "DELETE FROM drafts WHERE draft_id=?" [dId]
  return ()

insertPost :: Post -> IO ()
insertPost Post{Draft.postId = pId, draftText = dText} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO drafts(post_id, draft_text) values (?,?)"
           (pId, dText) 
  return ()
-}
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
   "UPDATE posts SET creation_time = d.creation_time,\
                     \category_id = d.category_id,\
                     \tags = d.tags,\
                     \text_content = d.text_content,\
                     \main_photo = d.main_photo,\
                     \additional_photos = d.additional_photos\
                 \FROM (SELECT creation_time, category_id, tags, text_content, main_photo, additional_photos\
                 \      FROM drafts WHERE draft_id=?) AS d" [dId]
  return ()

{-
UPDATE dummy
SET customer=subquery.customer,
    address=subquery.address,
    partn=subquery.partn
FROM (SELECT address_id, customer, address, partn
      FROM  /* big hairy SQL */ ...) AS subquery
WHERE dummy.address_id=subquery.address_id;
-}

{-
CREATE TABLE drafts (
  draft_id SERIAL PRIMARY KEY,
  post_id INTEGER REFERENCES posts,
  creation_time timestamp default current_timestamp,
  category_id INTEGER REFERENCES categories,
  tags text[],
  text_content text NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[],
  post_comments text[]
);
-}

updateDraft :: Draft -> IO ()
updateDraft Draft{ draftId = dId, Draft.postId = pId, Draft.authorId = aId
                 , Draft.creationTime = dTime, Draft.categoryId = cId
                 , Draft.tags = dTags, Draft.textContent = dText
                 , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "UPDATE drafts SET post_id=?, category_id=?, tags=?, text_content=?, main_photo=?, additional_photos=? WHERE draft_id=?"
           (pId, cId, dTags, dText, dPhoto, dAddPhotos, dId)
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
data Draft = Draft { draftId, postId, authorId :: Integer, creationTime :: T.LocalTimeStamp
                   , authorId :: Integer, categoryId :: Integer
                   , tags :: Maybe [Text], textContent :: Text, mainPhoto :: Text
                   , additionalPhotos :: Maybe [Text]
-}

insertDraft :: Draft -> IO ()
insertDraft Draft{ Draft.postId = pId, Draft.authorId = aId
                 , Draft.creationTime = dTime, Draft.categoryId = cId
                 , Draft.tags = dTags, Draft.textContent = dText
                 , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "news-server"
  , connectUser = "news-server"
  , connectPassword = "news-server" 
  }
  execute conn "INSERT INTO drafts(post_id, category_id, tags, text_content, main_photo, additional_photos) values (?, ?, ?, ?, ?, ?, ?)"
           (pId, cId, dTags, dText, dPhoto, dAddPhotos)
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