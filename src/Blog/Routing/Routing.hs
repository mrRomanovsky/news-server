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
  addPostRouter (reqPath ["posts", "*", "comments", "delete"]) deleteComment $
  addPostRouter (reqPath ["posts", "*", "comments"]) createComment $
  addPostRouter (reqPath ["users", "delete"]) deleteUser $
  addPostRouter (reqPath ["users"]) createUser $
  addPostRouter (reqPath ["tags", "update"]) updateTag $
  addPostRouter (reqPath ["tags", "delete"]) deleteTag $
  addPostRouter (reqPath ["tags"]) createTag $
  addPostRouter (reqPath ["drafts", "publish"]) publishDraft $
  addPostRouter (reqPath ["drafts", "update"]) updateDraft $
  addPostRouter (reqPath ["drafts", "delete"]) deleteDraft $
  addPostRouter (reqPath ["drafts"]) createDraft $
  addPostRouter (reqPath ["categories", "update"]) updateCategory $
  addPostRouter (reqPath ["categories", "delete"]) deleteCategory $
  addPostRouter (reqPath ["categories"]) createCategory $
  addPostRouter (reqPath ["authors", "update"]) updateAuthor $
  addPostRouter (reqPath ["authors", "delete"]) deleteAuthor $
  addPostRouter (reqPath ["authors"]) createAuthor $
  addGetRouter (reqPath ["posts", "*", "comments"]) getComments $
  addGetRouter (reqPath ["drafts"]) getDrafts $
  addGetRouter (reqPath ["users"]) getUsers $
  addGetRouter (reqPath ["posts"]) getPosts $
  addGetRouter (reqPath ["categories"]) getCategories $
  addGetRouter (reqPath ["tags"]) getTags $
  addGetRouter (reqPath ["authors"]) getAuthors defaultRouter

reqPath :: [Text] -> Request -> Bool
reqPath path = matchPath path . pathInfo

matchPath :: [Text] -> [Text] -> Bool
matchPath [] [] = True
matchPath _ [] = False
matchPath (x:xs) (y:ys)
  | x == "*" || x == y = matchPath xs ys
  | otherwise = False
