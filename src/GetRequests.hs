{-# LANGUAGE OverloadedStrings #-}

module GetRequests (processGetRequest, processFilterGetRequest) where

import Prelude hiding (read)
import qualified Tag as T
import qualified PostDTO as P
import qualified Post as PS
import qualified Draft as D
import qualified User as U
import qualified Author as A
import qualified Category as C
import Model
import Control.Monad (join)
import DbRequests
import Data.Text hiding (head)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200, status422, hAuthorization, Query)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple hiding (Query)

processGetRequest :: Request -> Connection -> IO Response
processGetRequest request =
  let mPage = lookup "page" $ queryString request
      sortBy = join $ lookup "sort_by" $ queryString request
      page = mPage >>=
        maybe Nothing (decode . B.fromStrict:: BS.ByteString -> Maybe Integer)
      in case pathInfo request of
  ["drafts"]     -> fmap respondJson . (read page sortBy :: Connection -> IO [D.Draft])
  ["tags"]       -> fmap respondJson . (read page sortBy :: Connection -> IO [T.Tag])
  ["categories"] -> fmap respondJson . (read page sortBy :: Connection -> IO [C.Category])
  ["users"]      -> fmap respondJson . (read page sortBy :: Connection -> IO [U.User])
  ["authors"]    ->
    let auth = lookup hAuthorization $ requestHeaders request
        in authResponse auth $ fmap respondJson . (read page sortBy :: Connection -> IO [A.Author])
  ["posts"]      -> fmap respondJson .
   (\c -> dtosToPosts c $ read page sortBy c)
  ["posts", n, "comments"] -> fmap respondJson . P.getPostComments n page

dtosToPosts :: Connection -> IO [P.PostDTO] -> IO [PS.Post]
dtosToPosts c dtosIO = do
  dtos <- dtosIO
  sequence $ PS.dtoToPost c <$> dtos

processFilterGetRequest :: Request -> Connection -> IO Response
processFilterGetRequest request c = 
  let path = pathInfo request --add path processing!
      queryStr = queryString request
      mPage = lookup "page" queryStr
      page = mPage >>=
        maybe Nothing (decode . B.fromStrict:: BS.ByteString -> Maybe Integer)
      in case head queryStr of
        ("author_name", Just author) ->
          respondJson <$> (dtosToPosts c $ P.getPostsByAuthor author page c)
        ("content_substr", Just author) ->
          respondJson <$> (dtosToPosts c $ P.getPostsWithSubstrInContent author page c)
        ("name_substr", Just author) ->
          respondJson <$> dtosToPosts c (P.getPostsWithSubstrInName author page c)
        ("tag", Just tag) -> 
          respondJson <$> (dtosToPosts c $ P.getPostsWithTag tag page c)
        ("tags_in", Just tagsIn) ->
          respondJson <$> (dtosToPosts c $ P.getPostsTagsIn tagsIn page c)
        ("tags_all", Just tagsAll) ->
          respondJson <$> (dtosToPosts c $ P.getPostsTagsAll tagsAll page c)
        ("created_at", Just date) ->
          respondJson <$> (dtosToPosts c $ P.getPostsDate date page c)
        ("created_at__lt", Just dateLt) ->
          respondJson <$> (dtosToPosts c $ P.getPostsDateLt dateLt page c)
        ("created_at__gt", Just dateGt) ->
          respondJson <$> (dtosToPosts c $ P.getPostsDateGt dateGt page c)
        ("category", Just cat) ->
          respondJson <$> (dtosToPosts c $ P.getPostsByCategory cat page c)
        ("substr", Just substr) ->
          respondJson <$> (dtosToPosts c $ P.getPostsBySubstr substr page c)

respondJson :: ToJSON m => [m] -> Response
respondJson = responseLBS status200 [("Content-Type", "application/json")] . encode