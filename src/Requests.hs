{-# LANGUAGE OverloadedStrings #-}

module Requests (processAppRequest, processPostRequest) where

import qualified Tag as T
import qualified Post as P
import qualified User as U
import qualified Author as A
import qualified Category as C
import DbRequests
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Attoparsec.Text
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.FromRow

data AppRequest = AppUsersRequest UsersRequest | AppAuthorsRequest AuthorsRequest |
                  AppCategoriesRequest CategoriesRequest | AppPostsRequest PostsRequest |
                  AppCommentsRequest CommentsRequest | AppTagsRequest TagsRequest

data UsersRequest = CreateUser U.User | GetUsers

data AuthorsRequest = CreateAuthor A.Author | GetAuthors | UpdateAuthor A.Author A.Author

data CategoriesRequest = CreateCategory C.Category | GetCategories | UpdateCategory C.Category C.Category

data PostsRequest = GetPosts | GetPostsBy (P.Post -> Bool)

data CommentsRequest = GetCommentsForPost P.Post | AddCommentForPost P.Post B.ByteString | DeleteCommentForPost P.Post Int

data TagsRequest = GetTags

processPostRequest :: Request -> IO Response
processPostRequest r = case pathInfo r of
  ["users"] -> postUser =<< strictRequestBody r
  ["users", "delete"] -> deleteUserBs =<< strictRequestBody r
  ["tags"] -> postTag =<< strictRequestBody r
  ["tags", "delete"] -> deleteTagBs =<< strictRequestBody r
  _         -> return notImplementedFeature

deleteTagBs :: B.ByteString -> IO Response
deleteTagBs b = do
  let tId = (parseOnly decimal $ decodeUtf8 $ B.toStrict b) :: Either String Integer
  either (\e -> print $ "error parsing tag id: " ++ e) deleteTag tId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Tag was successfully deleted from the database"

deleteUserBs :: B.ByteString -> IO Response
deleteUserBs b = do
  let uId = (parseOnly decimal $ decodeUtf8 $ B.toStrict b) :: Either String Integer
  either (\e -> print $ "error parsing user id: " ++ e) deleteUser uId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "User was successfully deleted from the database"

postTag :: B.ByteString -> IO Response
postTag b = do
  let tag = eitherDecode b :: Either String T.Tag
  either (\e -> print $ "error parsing tag: " ++ e) insertTag tag
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Tag was successfully added to the database"


postUser :: B.ByteString -> IO Response
postUser b = do
  let user = eitherDecode b :: Either String U.User
  either (\e -> print $ "error parsing user: " ++ e) insertUser user
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "User was successfully added to the database"

processAppRequest :: [Text] -> IO Response
processAppRequest path = case parseAppRequest path of
  (Just (AppUsersRequest usersRequest)) -> processUsersRequest usersRequest
  (Just (AppPostsRequest postsRequest)) -> processPostsRequest postsRequest
  (Just (AppCategoriesRequest categoriesRequest)) -> processCategoriesRequest categoriesRequest
  (Just (AppTagsRequest tagsRequest)) -> processTagsRequest tagsRequest
  Nothing                               -> return notImplementedFeature

parseAppRequest :: [Text] -> Maybe AppRequest
parseAppRequest ["users"] = Just $ AppUsersRequest GetUsers
parseAppRequest ["posts"] = Just $ AppPostsRequest GetPosts
parseAppRequest ["categories"] = Just $ AppCategoriesRequest GetCategories
parseAppRequest ["tags"] = Just $ AppTagsRequest GetTags
parseAppRequest _         = Nothing

user1 = U.User 1 "Test User 1" "Test Surname 1" "Test/avatar/path/img.jpg" (U.getLocTimestamp "2017-07-28 14:14:14") False

tempUsers = [ user1, user1{U.userId = 2, U.name = "Test User 2", U.surname = "Test Surname 2"}
            , user1{U.userId = 3, U.name = "Test User 3", U.surname = "Test Surname 3"}]

processUsersRequest :: UsersRequest -> IO Response
processUsersRequest (CreateUser _) = return notImplementedFeature
processUsersRequest GetUsers = respondJson <$> getUsers

processAuthorsRequest :: AuthorsRequest -> IO Response
processAuthorsRequest GetAuthors = respondJson <$> getAuthors
processAuthorsRequest _ = return notImplementedFeature

processCategoriesRequest :: CategoriesRequest -> IO Response
processCategoriesRequest GetCategories = respondJson <$> getCategories
processCategoriesRequest _ = return notImplementedFeature

processPostsRequest :: PostsRequest -> IO Response
processPostsRequest GetPosts = respondJson <$> getPosts
processPostsRequest _ = return notImplementedFeature

processCommentsRequest :: CommentsRequest -> IO Response
processCommentsRequest _ = return notImplementedFeature

processTagsRequest :: TagsRequest -> IO Response
processTagsRequest GetTags = respondJson <$> getTags

respondJson :: ToJSON m => [m] -> Response
respondJson = responseLBS status200 [("Content-Type", "application/json")] . encode

notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"