{-# LANGUAGE OverloadedStrings #-}

module Requests (processAppRequest) where

import qualified Tag as T
import qualified Post as P
import qualified User as U
import qualified Author as A
import qualified Category as C
import DbRequests
import Data.Text
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.FromRow

----type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

data AppRequest = AppUsersRequest UsersRequest | AppAuthorsRequest AuthorsRequest |
                  AppCategoriesRequest CategoriesRequest | AppPostsRequest PostsRequest |
                  AppCommentsRequest CommentsRequest | AppTagsRequest TagsRequest

data UsersRequest = CreateUser U.User | GetUsers

data AuthorsRequest = CreateAuthor A.Author | GetAuthors | UpdateAuthor A.Author A.Author

data CategoriesRequest = CreateCategory C.Category | GetCategories | UpdateCategory C.Category C.Category

data PostsRequest = GetPosts | GetPostsBy (P.Post -> Bool)

data CommentsRequest = GetCommentsForPost P.Post | AddCommentForPost P.Post B.ByteString | DeleteCommentForPost P.Post Int

data TagsRequest = GetTags

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

--parseLocalTimestamp

user1 = U.User 1 "Test User 1" "Test Surname 1" "Test/avatar/path/img.jpg" (U.getLocTimestamp "2017-07-28 14:14:14") False

tempUsers = [ user1, user1{U.userId = 2, U.name = "Test User 2", U.surname = "Test Surname 2"}
            , user1{U.userId = 3, U.name = "Test User 3", U.surname = "Test Surname 3"}]

processUsersRequest :: UsersRequest -> IO Response
processUsersRequest (CreateUser _) = return notImplementedFeature
processUsersRequest GetUsers = respondJson <$> getUsers
--  responseLBS status200 [("Content-Type", "application/json")] . encode <$> getUsers

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
--processTagsRequest _ = return notImplementedFeature

respondJson :: ToJSON m => [m] -> Response
respondJson = responseLBS status200 [("Content-Type", "application/json")] . encode

notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"

--data TagsRequest = CreateTag Tag | UpdateTag Tag Tag | 