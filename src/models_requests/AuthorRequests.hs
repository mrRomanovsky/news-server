{-# LANGUAGE OverloadedStrings #-}

module AuthorRequests where

import qualified Author as A
import qualified Category as C
import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (filter, head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple hiding (Query)
import DbRequests
import qualified Draft as D
import Model
import Network.HTTP.Types (Query, hAuthorization, status200, status422)
import Network.Wai
import qualified Post as PS
import qualified PostDTO as P
import Prelude hiding (read)
import qualified Tag as T
import RequestsUtils

getAuthors :: Request -> Connection -> IO Response
getAuthors request =
  let (page, sortBy, auth) = getAdditionalParams request
      in authResponse auth $
              fmap respondJson .
              (read page sortBy :: Connection -> IO [A.Author])

createAuthor :: Request -> Connection -> IO Response
createAuthor request = authorizeRequest request $
  actionWithBody request createAuthorBs

createAuthorBs :: B.ByteString -> Connection -> IO Response
createAuthorBs = createModel . decodeAuthor

deleteAuthor :: Request -> Connection -> IO Response
deleteAuthor request = authorizeRequest request $
  actionWithBody request deleteAuthorBs

deleteAuthorBs :: B.ByteString -> Connection -> IO Response
deleteAuthorBs = deleteModel . (eitherDecode :: B.ByteString -> Either String A.AuthorId)

updateAuthor :: Request -> Connection -> IO Response
updateAuthor request = authorizeRequest request $
  actionWithBody request updateAuthorBs

updateAuthorBs :: B.ByteString -> Connection -> IO Response
updateAuthorBs = updateModel . decodeAuthor