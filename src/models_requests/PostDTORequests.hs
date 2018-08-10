{-# LANGUAGE OverloadedStrings #-}

module PostDTORequests where

import Data.Aeson
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Model
import Network.Wai
import Prelude hiding (read)
import qualified PostDTO as PD
import qualified Post as P
import DbRequests
import RequestsUtils

getPostsSimple :: Request -> Connection -> IO Response
getPostsSimple request = 
  let (page, sortBy, _) = getAdditionalParams request
      in fmap respondJson .
          (\c -> dtosToPosts c $ read page sortBy c)

dtosToPosts :: Connection -> IO [PD.PostDTO] -> IO [P.Post]
dtosToPosts c dtosIO = do
  dtos <- dtosIO
  sequence $ P.dtoToPost c <$> dtos

getPostsFiltered :: Request -> Connection -> IO Response
getPostsFiltered request = 
  let (page, _, _) = getAdditionalParams request
      queryStr = queryString request
    in case head queryStr of
      ("author_name", Just author) -> getPostsBy PD.getPostsByAuthor author page
      ("content_substr", Just sub) -> getPostsBy PD.getPostsWithSubstrInContent sub page
      ("name_substr", Just sub) -> getPostsBy PD.getPostsWithSubstrInName sub page
      ("tag", Just tag) -> getPostsBy PD.getPostsWithTag tag page
      ("tags_in", Just tags) -> getPostsBy PD.getPostsTagsIn tags page
      ("tags_all", Just tags) -> getPostsBy PD.getPostsTagsAll tags page
      ("created_at", Just date) -> getPostsBy PD.getPostsDate date page 
      ("created_at__lt", Just date) -> getPostsBy PD.getPostsDateLt date page 
      ("created_at__gt", Just date) -> getPostsBy PD.getPostsDateGt date page
      ("category", Just cat) -> getPostsBy PD.getPostsByCategory cat page
      ("substr", Just sub) -> getPostsBy PD.getPostsBySubstr sub page
      _ -> const $ return notFound

getPostsBy :: (BS.ByteString -> Maybe Integer -> Connection -> IO [PD.PostDTO]) -> BS.ByteString -> Maybe Integer -> Connection -> IO Response
getPostsBy getFilt param page c = respondJson <$> dtosToPosts c (getFilt param page c)