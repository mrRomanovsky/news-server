{-# LANGUAGE OverloadedStrings #-}

module AuthorTests where

import Control.Monad.Except
import Data.Maybe (isNothing)
import TestUtils
import Author
import Model

authorTests =
  [ ("testCreateAuthor", testCreateAuthor)
  , ("testReadAuthor", testReadAuthor)
  , ("testUpdateAuthor", testUpdateAuthor)
  , ("testDeleteAuthor", testDeleteAuthor)
  ]

testAuthor = Author (AuthorId 1) 1 $ Just "Author Description"

testCreateAuthor :: Either String Bool
testCreateauthor = do
  return False

testUpdateAuthor :: Either String Bool
testUpdateauthor = do
  return False

testDeleteAuthor :: Either String Bool
testCreateauthor = do
  return False