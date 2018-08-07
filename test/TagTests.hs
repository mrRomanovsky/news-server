{-# LANGUAGE OverloadedStrings #-}

module TagTests where

import Control.Monad.Except
import Data.Vector
import Data.Maybe (isNothing)
import TestUtils
import Tag
import Model

TagTests =
  [ ("testCreateTag", testCreateTag)
  , ("testReadTag", testReadTag)
  , ("testUpdateTag", testUpdateTag)
  , ("testDeleteTag", testDeleteTag)
  ]

{-
data Tag = Tag {tagId :: TagId, tagName :: Text} deriving (Show, Generic)
-}

testTag = Tag (TagId 1) "Tag Name"

testCreateTag :: Either String Bool
testCreateTag = do
  return False

testUpdateTag :: Either String Bool
testUpdateTag = do
  return False

testDeleteTag :: Either String Bool
testCreateTag = do
  return False