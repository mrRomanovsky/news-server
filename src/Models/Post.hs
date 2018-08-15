{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Post where

import Models.Author
import Models.Category
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import Data.String (fromString)
import Data.Text hiding (head, takeWhile)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector hiding ((++), head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Database.PostgreSQL.Simple.ToField
import ServerDB.DbRequests
import GHC.Generics
import Models.Model
import qualified Models.PostDTO as DT
import Prelude hiding (takeWhile)
import Models.Tag
import Models.User

data Post = Post
  { postId :: Integer
  , postName :: Text
  , creation_date :: T.LocalTimestamp
  , author :: Author
  , category :: Category
  , tags :: Maybe (Vector Tag)
  , text :: Text
  , mainPhoto :: Text
  , additionalPhotos :: Maybe (Vector Text)
  , comments :: Maybe (Vector Text)
  } deriving (Show, Generic)

instance ToJSON Post

dtoToPost :: Connection -> DT.PostDTO -> IO Post
dtoToPost c dto = do
  authors <-
    query c "SELECT * FROM authors WHERE author_id = ?" [DT.authorId dto] :: IO [Author]
  categories <-
    query c "SELECT * FROM categories WHERE category_id = ?" [DT.categoryId dto] :: IO [Category]
  tagsQ <-
    query c "SELECT * FROM tags WHERE ARRAY[tag_id] <@ ?" [DT.tags dto] :: IO [Tag]
  return
    Post
      { postId = DT.pId $ DT.postId dto
      , postName = DT.postName dto
      , creation_date = DT.creation_date dto
      , author = head authors
      , category = head categories
      , tags = Just $ fromList tagsQ
      , text = DT.text dto
      , mainPhoto = DT.mainPhoto dto
      , additionalPhotos = DT.additionalPhotos dto
      , comments = DT.comments dto
      }
