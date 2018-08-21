{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Blog.Handlers.Post
  ( getPosts
  ) where

import Blog.Handlers.HandlersUtils
import Blog.Models.Model
import qualified Blog.Models.Post as P
import qualified Blog.Models.PostDTO as PD
import Control.Monad (join)
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Network.Wai
import Prelude hiding (read)

getPosts :: Request -> Connection -> IO Response
getPosts =
  filterBy
    [ ("author_name", PD.getPostsByAuthor)
    , ("content_substr", PD.getPostsWithSubstrInContent)
    , ("name_substr", PD.getPostsWithSubstrInName)
    , ("tag", PD.getPostsWithTag)
    , ("tags_in", PD.getPostsTagsIn)
    , ("tags_all", PD.getPostsTagsAll)
    , ("created_at", PD.getPostsDate)
    , ("created_at__lt", PD.getPostsDateLt)
    , ("created_at__gt", PD.getPostsDateGt)
    , ("category", PD.getPostsByCategory)
    , ("substr", PD.getPostsBySubstr)
    ]

filterBy ::
     [( BS.ByteString
      , BS.ByteString -> Maybe Integer -> Connection -> IO [PD.PostDTO])]
  -> Request
  -> Connection
  -> IO Response
filterBy filters request =
  let query = queryString request
      filt = foldr getFilter Nothing filters
      getFilter (p, f) Nothing = (f, ) <$> join (lookup p query)
      getFilter _ f = f
   in maybe
        (getPostsUnfiltered request)
        (\(f, p) -> getPostsBy f p request)
        filt

getPostsBy ::
     (BS.ByteString -> Maybe Integer -> Connection -> IO [PD.PostDTO])
  -> BS.ByteString
  -> Request
  -> Connection
  -> IO Response
getPostsBy filtGet param request c =
  let (page, _, _) = getAdditionalParams request
   in respondJson <$> dtosToPosts c (filtGet param page c)

getPostsUnfiltered :: Request -> Connection -> IO Response
getPostsUnfiltered request =
  let (page, sortBy, _) = getAdditionalParams request
   in fmap respondJson . (\c -> dtosToPosts c $ getData page sortBy c)

dtosToPosts :: Connection -> IO [PD.PostDTO] -> IO [P.Post]
dtosToPosts c dtosIO = do
  dtos <- dtosIO
  sequence $ P.dtoToPost c <$> dtos
