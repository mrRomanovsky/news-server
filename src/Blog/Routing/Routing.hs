{-# LANGUAGE OverloadedStrings #-}

module Blog.Routing.Routing
  ( routers
  ) where

import Blog.Handlers.Author
import Blog.Handlers.Category
import Blog.Handlers.Comment
import Blog.Handlers.Draft
import Blog.Handlers.Post
import Blog.Handlers.Tag
import Blog.Handlers.User
import Blog.Routing.Router
import Data.Text
import Network.HTTP.Types (Query, methodGet, methodPost)
import Network.Wai
import Network.Wai.Handler.Warp (run)

routers :: Router
routers =
  addPostRouter (postPath ["posts", "*", "comments", "delete"]) deleteComment $
  addPostRouter (postPath ["posts", "*", "comments"]) createComment $
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
  addGetRouter (notFilteredGet ["posts", "*", "comments"]) getComments $
  addGetRouter (filteredGetTo ["posts"]) getPostsFiltered $
  addGetRouter (notFilteredGet ["drafts"]) getPostsSimple $
  addGetRouter (notFilteredGet ["users"]) getPostsSimple $
  addGetRouter (notFilteredGet ["posts"]) getPostsSimple $
  addGetRouter (notFilteredGet ["categories"]) getCategories $
  addGetRouter (notFilteredGet ["tags"]) getTags $
  addGetRouter (notFilteredGet ["authors"]) getAuthors defaultRouter

postPath :: [Text] -> Request -> Bool
postPath path = matchPath path . pathInfo

filteredGetTo :: [Text] -> Request -> Bool
filteredGetTo path request =
  not (notFiltered $ queryString request) && pathInfo request == path

notFilteredGet :: [Text] -> Request -> Bool
notFilteredGet path request =
  notFiltered (queryString request) && matchPath path (pathInfo request)

notFiltered :: Query -> Bool
notFiltered [] = True
notFiltered (("page", p):qs) = True
notFiltered (("sort_by", sB):qs) = True
notFiltered _ = False

matchPath :: [Text] -> [Text] -> Bool
matchPath [] [] = True
matchPath _ [] = False
matchPath (x:xs) (y:ys)
  | x == "*" || x == y = matchPath xs ys
  | otherwise = False
