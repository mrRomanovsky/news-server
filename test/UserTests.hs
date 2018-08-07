{-# LANGUAGE OverloadedStrings #-}

module UserTests where

import Control.Monad.Except
import Data.Vector
import Data.Maybe (isNothing)
import TestUtils
import User
import Model

UserTests =
  [ ("testCreateUser", testCreateUser)
  , ("testReadUser", testReadUser)
  , ("testUpdateUser", testUpdateUser)
  , ("testDeleteUser", testDeleteUser)
  ]

{-
data User = User {userId :: UserId,
                  name :: Text,
                  surname :: Text,
                  avatar :: Text,
                  creationTime :: T.LocalTimestamp,
                  isAdmin :: Bool} deriving (Show, Generic)
-}

testUser = User (UserId 1) "User Name" "User Surname" "User Avatar" (getLocTimestamp "2017-07-28 14:14:14") True

testCreateUser :: Either String Bool
testCreateUser = do
  return False

testUpdateUser :: Either String Bool
testUpdateUser = do
  return False

testDeleteUser :: Either String Bool
testCreateUser = do
  return False