{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Category where

import Model
import Data.Aeson
import Control.Monad
import DbRequests
import Control.Applicative
import GHC.Generics
import Data.Text
import Data.Vector
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

data Category = Category {categoryId :: CategoryId, name :: Text, nestedCategories :: Maybe (Vector Integer)} deriving (Show, Generic)

newtype CategoryId = CategoryId {cId :: Integer}

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
  create Category{Category.name = n, Category.nestedCategories = pId} conn = do
    execute conn "INSERT INTO categories(category_name, category_nested_categories) values (?,?)"
      (n, pId)
    return ()

  read = getRecords "categories"

  update Category{Category.categoryId = cId, Category.name = n, Category.nestedCategories = pId} conn = do
    execute conn "UPDATE categories SET category_name=?, category_nested_categories=? WHERE category_id=?"
             (n, pId, cId)
    return ()
    
  delete cId conn = do
    execute conn "DELETE FROM categories WHERE category_id=?" [cId]
    return ()

instance FromJSON Category where
  parseJSON (Object v) = Category <$> (v .: "categoryId" <|> pure (CategoryId (-1))) <*> v .: "name" <*> v .:? "parentId"
  parseJSON _ = mzero
instance ToJSON Category

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field