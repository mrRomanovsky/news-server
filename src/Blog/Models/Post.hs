{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Models.Post where

import Blog.Models.Author
import Blog.Models.Category
import qualified Blog.Models.PostDTO as DT
import Blog.Models.Tag
import Data.Aeson
import Data.Text hiding (head, takeWhile)
import Data.Vector hiding ((++), head)
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.Time as T
import GHC.Generics
import Prelude hiding (takeWhile)

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
