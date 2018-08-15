{-# LANGUAGE OverloadedStrings #-}

module Blog.ModelsRequests.RequestsUtils where

import qualified Blog.Models.Author as A
import qualified Blog.Models.Category as C
import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (filter, head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple hiding (Query)
import Blog.ServerDB.DbRequests
import qualified Blog.Models.Draft as D
import Blog.Models.Model
import Network.HTTP.Types (Query, hAuthorization, status200, status422)
import Network.Wai
import qualified Blog.Models.Post as PS
import qualified Blog.Models.PostDTO as P
import Prelude hiding (read)
import qualified Blog.Models.Tag as T
import qualified Blog.Models.User as U

actionWithBody ::
     Request
  -> (B.ByteString -> Connection -> IO Response)
  -> Connection
  -> IO Response
actionWithBody request act c = strictRequestBody request >>= (`act` c)

authorizeRequest ::
     Request -> (Connection -> IO Response) -> Connection -> IO Response
authorizeRequest request =
  let auth = lookup hAuthorization $ requestHeaders request
   in authResponse auth

respondJson :: ToJSON m => [m] -> Response
respondJson =
  responseLBS status200 [("Content-Type", "application/json")] . encode

getAdditionalParams ::
     Request -> (Maybe Integer, Maybe BS.ByteString, Maybe BS.ByteString)
getAdditionalParams request =
  let sortBy = join $ lookup "sort_by" $ queryString request
      mPage = lookup "page" $ queryString request
      page =
        mPage >>=
        maybe Nothing (decode . B.fromStrict :: BS.ByteString -> Maybe Integer)
      auth = lookup hAuthorization $ requestHeaders request
   in (page, sortBy, auth)

decodeUser = eitherDecode :: B.ByteString -> Either String U.User

decodeAuthor = eitherDecode :: B.ByteString -> Either String A.Author

decodeTag = eitherDecode :: B.ByteString -> Either String T.Tag

decodeCategory = eitherDecode :: B.ByteString -> Either String C.Category

decodeDraft = eitherDecode :: B.ByteString -> Either String D.Draft

updateModel :: Model m id => Either String m -> Connection -> IO Response
updateModel model conn =
  either processBadModel ((>> return responseUpdated) . (`update` conn)) model

responseUpdated =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully updated"

deleteModel :: Model m id => Either String id -> Connection -> IO Response
deleteModel mId conn =
  either processBadModel ((>> return responseDeleted) . (`delete` conn)) mId

responseDeleted =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully deleted from the database"

createModel :: Model m id => Either String m -> Connection -> IO Response
createModel model conn =
  either processBadModel ((>> return responseAdded) . (`create` conn)) model

responseAdded =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully added to the database"

modelError :: Response
modelError =
  responseLBS status422 [("Content-Type", "application/json")] "Invalid model!"

processBadModel :: String -> IO Response
processBadModel e = do
  appendFile "news-server.log" $ "\nerror processing request body: " ++ e
  return errorResponse

errorResponse :: Response
errorResponse =
  responseLBS status422 [("Content-Type", "text/plain")] "Bad model/id data!"
