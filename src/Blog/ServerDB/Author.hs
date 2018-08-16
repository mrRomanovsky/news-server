{-# LANGUAGE OverloadedStrings #-}

module Blog.ServerDB.Author
  ( getDraftAuthor
  , getAuthorId
  ) where

import qualified Data.ByteString as B
import Database.PostgreSQL.Simple

getDraftAuthor :: Integer -> Connection -> IO Integer
getDraftAuthor dId conn = do
  authorId <- query conn "SELECT author_id FROM drafts WHERE draft_id = ?" [dId]
  case authorId of
    [[aId]] -> return aId
    _ -> error "draft didn't have an author!"

getAuthorId :: B.ByteString -> Connection -> IO (Maybe Integer)
getAuthorId uId conn = do
  authorId <-
    query conn "SELECT author_id FROM authors WHERE \"user_id\" = ?" [uId]
  case authorId of
    [[aId]] -> return $ Just aId
    _ -> return Nothing
