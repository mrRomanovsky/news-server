{-# LANGUAGE OverloadedStrings #-}

module CategoryTests where

import Control.Monad.Except
import Data.Vector
import Data.Maybe (isNothing)
import TestUtils
import Category
import Model

categoryTests =
  [ ("testCreateCategory", testCreateCategory)
  , ("testReadCategory", testReadCategory)
  , ("testUpdateCategory", testUpdateCategory)
  , ("testDeleteCategory", testDeleteCategory)
  ]

{-
data Category = Category {categoryId :: CategoryId, name :: Text, nestedCategories :: Maybe (Vector Integer)} deriving (Show, Generic)
-}

testCategory = Category (CategoryId 1) "Category Name" $ Just $ fromList [2, 3, 4]

testCreateCategory :: Either String Bool
testCreateCategory = do
  return False

testUpdateCategory :: Either String Bool
testUpdateCategory = do
  return False

testDeleteCategory :: Either String Bool
testCreateCategory = do
  return False