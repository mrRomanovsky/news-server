{-# LANGUAGE OverloadedStrings #-}

module Routing.Routing
  ( routers
  ) where

import ModelsRequests.AuthorRequests
import ModelsRequests.CategoryRequests
import ModelsRequests.CommentRequests
import Data.Text
import ModelsRequests.DraftRequests
import Network.HTTP.Types (Query, methodGet, methodPost)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import ModelsRequests.PostRequests
import Routing.Router
import ModelsRequests.TagRequests
import ModelsRequests.UserRequests

routers :: Router
routers =
  addPostRouter isCommentsDelete deleteComment $
  addPostRouter isCommentsRequest createComment $
  addGetRouter isCommentsRequest getComments $
  addPostRouter (postPath ["users", "delete"]) deleteUser $
  addPostRouter (postPath ["users"]) createUser $
  addPostRouter (postPath ["tags", "update"]) updateTag $
  addPostRouter (postPath ["tags", "delete"]) deleteTag $
  addPostRouter (postPath ["tags"]) createTag $
  addPostRouter (postPath ["drafts", "publish"]) publishDraft $
  addPostRouter (postPath ["drafts", "update"]) updateDraft $
  addPostRouter (postPath ["drafts", "delete"]) deleteDraft $
  addPostRouter (postPath ["drafts"]) createDraft $
  addPostRouter (postPath ["categories", "update"]) updateCategory $
  addPostRouter (postPath ["categories", "delete"]) deleteCategory $
  addPostRouter (postPath ["categories"]) createCategory $
  addPostRouter (postPath ["authors", "update"]) updateAuthor $
  addPostRouter (postPath ["authors", "delete"]) deleteAuthor $
  addPostRouter (postPath ["authors"]) createAuthor $
  addGetRouter (filteredGetTo ["posts"]) getPostsFiltered $
  addGetRouter (simpleGetTo ["drafts"]) getPostsSimple $
  addGetRouter (simpleGetTo ["users"]) getPostsSimple $
  addGetRouter (simpleGetTo ["posts"]) getPostsSimple $
  addGetRouter (simpleGetTo ["categories"]) getCategories $
  addGetRouter (simpleGetTo ["tags"]) getTags $
  addGetRouter (simpleGetTo ["authors"]) getAuthors defaultRouter

postPath :: [Text] -> Request -> Bool
postPath path = (== path) . pathInfo

filteredGetTo :: [Text] -> Request -> Bool
filteredGetTo path request =
  not (isSimpleGet $ queryString request) && pathInfo request == path

simpleGetTo :: [Text] -> Request -> Bool
simpleGetTo path request =
  isSimpleGet (queryString request) && pathInfo request == path

isSimpleGet :: Query -> Bool
isSimpleGet [] = True
isSimpleGet (("page", p):qs) = True
isSimpleGet (("sort_by", sB):qs) = True
isSimpleGet _ = False

isCommentsDelete :: Request -> Bool
isCommentsDelete request =
  case pathInfo request of
    ["posts", pNumber, "comments", "delete"] -> True
    _ -> False

isCommentsRequest :: Request -> Bool
isCommentsRequest request =
  case pathInfo request of
    ["posts", pNumber, "comments"] -> True
    _ -> False
