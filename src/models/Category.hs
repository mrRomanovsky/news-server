{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Category where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Vector
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import DbRequests
import GHC.Generics
import Model

data Category = Category
  { categoryId :: CategoryId
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
  fromField field mdata = do
    x <- fromField field mdata
    return $ CategoryId x

instance ToField CategoryId where
  toField = toField . cId

instance Model Category CategoryId where
  create Category {Category.name = n, Category.nestedCategories = pId} conn =
    void $
    execute
      conn
      "INSERT INTO categories(category_name, category_nested_categories) values (?,?)"
      (n, pId)
  read = getRecords "categories"
  update Category { Category.categoryId = cId
                  , Category.name = n
                  , Category.nestedCategories = pId
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
    Category <$> (v .: "categoryId" <|> pure (CategoryId (-1))) <*> v .: "name" <*>
    v .:? "parentId"
  parseJSON _ = mzero

instance ToJSON Category

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field
