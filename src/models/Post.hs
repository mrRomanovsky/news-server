{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Post where

import Control.Applicative
import Prelude hiding (takeWhile)
import Author
import User
import Category
import Tag
import Model
import DbRequests
import Data.Aeson
import GHC.Generics
import Data.Text hiding (takeWhile)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Data.ByteString as B
import Data.Vector
import qualified Database.PostgreSQL.Simple.Time as T

data Post = Post { postId :: PostId, postName :: Text, creation_date :: T.LocalTimestamp
                 , authorId :: Integer, categoryId :: Integer, tags :: Maybe (Vector Integer)
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: Maybe (Vector Text)
                 , comments :: Maybe (Vector Text)} deriving (Show, Generic)

newtype PostId = PostId {pId :: Integer}

instance Show PostId where
  show = show . pId

instance ToJSON PostId where
  toJSON = toJSON . pId

instance FromField PostId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ PostId x

instance ToField PostId where
  toField = toField . pId

instance Model Post PostId where
  create = error "Sorry, this feature is not implemented yet"

  read = getRecords "posts"

  update = error "Sorry, this feature is not implemented yet"

  delete = error "Sorry, this feature is not implemented yet"

--instance FromJSON Post - not needed yet
instance ToJSON Post

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

getPostsByAuthor :: B.ByteString -> Connection -> IO [Post]
getPostsByAuthor author conn =
  query conn "SELECT * FROM posts \
             \WHERE author_id = (SELECT author_id FROM authors \
               \WHERE users_id = (SELECT users_id FROM users \
                 \WHERE users_name = ?))" [author]

getPostsWithSubstrInContent :: B.ByteString -> Connection -> IO [Post]
getPostsWithSubstrInContent substr conn =
  query conn "SELECT * FROM posts WHERE text_content LIKE ?"
    ["%" `B.append` substr `B.append` "%"]

getPostsWithSubstrInName :: B.ByteString -> Connection -> IO [Post]
getPostsWithSubstrInName substr conn =
  query conn "SELECT * FROM posts WHERE post_name LIKE ?"
    ["%" `B.append` substr `B.append` "%"]

getPostsWithTag :: B.ByteString -> Connection -> IO [Post]
getPostsWithTag tag conn =
  query conn "SELECT * FROM posts WHERE tags @> ?"
    ["{" `B.append` tag `B.append` "}"]

getPostsTagsIn :: B.ByteString -> Connection -> IO [Post]
getPostsTagsIn tagsIn conn = 
  query conn "SELECT * FROM posts WHERE tags && ?"
    ["{" `B.append` getArr tagsIn `B.append` "}"]

getPostsTagsAll :: B.ByteString -> Connection -> IO [Post]
getPostsTagsAll tagsAll conn =
  query conn "SELECT * FROM posts WHERE tags @> ?"
    ["{" `B.append` getArr tagsAll `B.append` "}"]

getArr :: B.ByteString -> B.ByteString
getArr = B.init . B.tail