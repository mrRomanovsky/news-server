{-# LANGUAGE OverloadedStrings #-}

module DraftTests where

import Control.Monad.Except
import Data.Vector
import Data.Maybe (isNothing)
import TestUtils
import Draft
import Model

draftTests =
  [ ("testCreateDraft", testCreateDraft)
  , ("testReadDraft", testReadDraft)
  , ("testUpdateDraft", testUpdateDraft)
  , ("testDeleteDraft", testDeleteDraft)
  ]

{-
data Draft = Draft { draftId :: DraftId, postId, authorId :: Integer, postName :: Text, creationTime :: T.LocalTimestamp
                   , categoryId :: Integer, tags :: Maybe (Vector Integer)
                   , textContent :: Text, mainPhoto :: Text
                   , additionalPhotos :: Maybe (Vector Text), postComments :: Maybe (Vector Text)}
-}

testDraft = Draft (DraftId 1) 1 1 "Draft Name" (getLocTimestamp "2017-07-28 14:14:14") 1 (Just $ fromList [2, 3, 4])
  "Draft Text Content" "Draft Main Photo" "Draft additional photos" Nothing

testCreateDraft :: Either String Bool
testCreateDraft = do
  return False

testUpdateDraft :: Either String Bool
testUpdateDraft = do
  return False

testDeleteDraft :: Either String Bool
testCreateDraft = do
  return False