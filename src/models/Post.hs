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
import qualified PostDTO as DT
import Model
import DbRequests
import Data.Aeson
import GHC.Generics
import Data.String (fromString)
import Data.Text hiding (takeWhile, head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Data.ByteString as B
import Data.Vector hiding ((++), head)
import qualified Database.PostgreSQL.Simple.Time as T

data Post = Post { postId :: Integer, postName :: Text, creation_date :: T.LocalTimestamp
                 , author :: Author, category :: Category, tags :: Maybe (Vector Tag)
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: Maybe (Vector Text)
                 , comments :: Maybe (Vector Text)} deriving (Show, Generic)

instance ToJSON Post

dtoToPost :: Connection -> DT.PostDTO -> IO Post
dtoToPost c dto = do
  authors <- query c "SELECT * FROM authors WHERE author_id = ?" [DT.authorId dto] :: IO [Author]
  categories <- query c "SELECT * FROM categories WHERE category_id = ?" [DT.categoryId dto] :: IO [Category]
  tagsQ <- query c "SELECT * FROM tags WHERE ARRAY[tag_id] <@ ?" [DT.tags dto] :: IO [Tag]
  return Post{ postId = DT.pId $ DT.postId dto, postName = DT.postName dto, creation_date = DT.creation_date dto
             , author = head authors, category = head categories, tags = Just $ fromList tagsQ
             , text = DT.text dto, mainPhoto = DT.mainPhoto dto, additionalPhotos = DT.additionalPhotos dto
             , comments = DT.comments dto }