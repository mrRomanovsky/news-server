{-# LANGUAGE OverloadedStrings #-}

module PostDTOTests where

import Control.Monad.Except
import Data.Vector
import Data.Maybe (isNothing)
import TestUtils
import PostDTO
import Model

PostDTOTests =
  [ ("testCreatePostDTO", testCreatePostDTO)
  , ("testReadPostDTO", testReadPostDTO)
  , ("testUpdatePostDTO", testUpdatePostDTO)
  , ("testDeletePostDTO", testDeletePostDTO)
  ]

{-
data PostDTO = PostDTO { postId :: PostId, postName :: Text, creation_date :: T.LocalTimestamp
                 , authorId :: Integer, categoryId :: Integer, tags :: Maybe (Vector Integer)
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: Maybe (Vector Text)
                 , comments :: Maybe (Vector Text)
-}

testPostDTO = PostDTO (PostId 1) "PostDTO Name" (getLocTimestamp "2017-07-28 14:14:14") 1 1 (Just $ fromList [2, 3, 4])
  "PostDTO Text Content" "PostDTO Main Photo" "PostDTO additional photos" Nothing

testCreatePostDTO :: Either String Bool
testCreatePostDTO = do
  return False

testUpdatePostDTO :: Either String Bool
testUpdatePostDTO = do
  return False

testDeletePostDTO :: Either String Bool
testCreatePostDTO = do
  return False