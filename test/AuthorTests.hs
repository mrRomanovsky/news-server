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

{-
data Author = Author {authorId :: AuthorId, userId :: Integer, desc :: Maybe Text}
-}

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
{-
import Control.Monad.Except
import Data.Maybe (isNothing)
import TelegramBot
import TelegramConfig
import TelegramJson
import TestUtils

telegramTests =
  [ ("testFindLastMessage", testFindLastMessage)
  , ("testProcessUpdates", testProcessUpdates)
  , ("testChangeRepeats", testChangeRepeats)
  ]

testChangeRepeats :: Either String Bool
testChangeRepeats = do
  let testName = "testChangeRepeats : "
  checkResult (testName ++ "test1") $
    repeats (config $ changeRepeats 4 testingBot1) == 4
  checkResult (testName ++ "test2") $
    repeats (config $ changeRepeats 2 testingBot1) == 2
  checkResult (testName ++ "test3") $
    repeats (config $ changeRepeats 0 testingBot1) == 0
-}