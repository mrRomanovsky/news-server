{-# LANGUAGE OverloadedStrings #-}

module Requests (processAppRequest) where

import qualified Post as P
import qualified User as U
import qualified Author as A
import qualified Category as C
import Data.Text
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as B

----type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

data AppRequest = AppUsersRequest UsersRequest | AppAuthorsRequest AuthorsRequest |
                  AppCategoriesRequest CategoriesRequest | AppPostsRequest PostsRequest |
                  AppCommentsRequest CommentsRequest

data UsersRequest = CreateUser U.User | GetUsers

data AuthorsRequest = CreateAuthor A.Author | GetAuthors | UpdateAuthor A.Author A.Author

data CategoriesRequest = CreateCategory C.Category | GetCategories | UpdateCategory C.Category C.Category

data PostsRequest = GetPosts | GetPostsBy (P.Post -> Bool)

data CommentsRequest = GetCommentsForPost P.Post | AddCommentForPost P.Post B.ByteString | DeleteCommentForPost P.Post Int

processAppRequest :: [Text] -> Response
processAppRequest path = case parseAppRequest path of
  (Just (AppUsersRequest usersRequest)) -> processUsersRequest usersRequest
  Nothing                               -> notImplementedFeature

parseAppRequest :: [Text] -> Maybe AppRequest
parseAppRequest ["users"] = Just $ AppUsersRequest GetUsers
parseAppRequest _         = Nothing

user1 = U.User "Test User 1" "Test Surname 1" "Test/avatar/path/img.jpg" "22.22.22" False

tempUsers = [ user1, user1{U.name = "Test User 2", U.surname = "Test Surname 2"}
            , user1{U.name = "Test User 3", U.surname = "Test Surname 3"}]

processUsersRequest :: UsersRequest -> Response
processUsersRequest (CreateUser _) = notImplementedFeature
processUsersRequest GetUsers = responseLBS status200 [("Content-Type", "application/json")] $ encode tempUsers

{-
application request respond = do
  print $ pathInfo request --array of path parts, divided with slashes, left to right
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello Wor
-}

processAuthorsRequest :: AuthorsRequest -> Response
processAuthorsRequest = undefined

processCategoriesRequest :: CategoriesRequest -> Response
processCategoriesRequest = undefined

processPostsRequest :: PostsRequest -> Response
processPostsRequest = undefined

processCommentsRequest :: CommentsRequest -> Response
processCommentsRequest = undefined

notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"

--data TagsRequest = CreateTag Tag | UpdateTag Tag Tag | 