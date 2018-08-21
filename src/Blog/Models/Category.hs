{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blog.Models.Category where

import Blog.Models.Model
import Blog.ServerDB.DbRequests
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Vector
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data Category = Category
  { categoryId :: Maybe CategoryId
  , name :: Text
  , nestedCategories :: Maybe (Vector Integer)
  } deriving (Show, Generic)

newtype CategoryId = CategoryId
  { cId :: Integer
  }

instance Show CategoryId where
  show = show . cId

instance ToJSON CategoryId where
  toJSON = toJSON . cId

instance FromJSON CategoryId where
  parseJSON = fmap CategoryId . parseJSON

instance FromField CategoryId where
  fromField field mdata = CategoryId <$> fromField field mdata

instance ToField CategoryId where
  toField = toField . cId

instance Model Category CategoryId where
  create Category {Blog.Models.Category.name = n, nestedCategories = pId} conn =
    void $
    execute
      conn
      "INSERT INTO categories(category_name, category_nested_categories) values (?,?)"
      (n, pId)
  getData = getRecords "categories"
  update Category { categoryId = cId
                  , Blog.Models.Category.name = n
                  , nestedCategories = pId
                  } conn =
    void $
    execute
      conn
      "UPDATE categories SET category_name=?, category_nested_categories=? WHERE category_id=?"
      (n, pId, cId)
  delete cId conn =
    void $ execute conn "DELETE FROM categories WHERE category_id=?" [cId]

instance FromJSON Category where
  parseJSON (Object v) =
    Category <$> v .:? "categoryId" <*> v .: "name" <*> v .:? "parentId"
  parseJSON _ = mzero

instance ToJSON Category

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field
