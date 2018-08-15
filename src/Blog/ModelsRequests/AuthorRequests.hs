module Blog.ModelsRequests.AuthorRequests
  ( getAuthors
  , createAuthor
  , deleteAuthor
  , updateAuthor
  ) where

import qualified Blog.Models.Author as A
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Blog.ServerDB.DbRequests
import Blog.Models.Model
import Network.Wai
import Prelude hiding (read)
import Blog.ModelsRequests.RequestsUtils

getAuthors :: Request -> Connection -> IO Response
getAuthors request =
  let (page, sortBy, auth) = getAdditionalParams request
   in authResponse auth $
      fmap respondJson . (read page sortBy :: Connection -> IO [A.Author])

createAuthor :: Request -> Connection -> IO Response
createAuthor request =
  authorizeRequest request $ actionWithBody request createAuthorBs

createAuthorBs :: B.ByteString -> Connection -> IO Response
createAuthorBs = createModel . decodeAuthor

deleteAuthor :: Request -> Connection -> IO Response
deleteAuthor request =
  authorizeRequest request $ actionWithBody request deleteAuthorBs

deleteAuthorBs :: B.ByteString -> Connection -> IO Response
deleteAuthorBs =
  deleteModel . (eitherDecode :: B.ByteString -> Either String A.AuthorId)

updateAuthor :: Request -> Connection -> IO Response
updateAuthor request =
  authorizeRequest request $ actionWithBody request updateAuthorBs

updateAuthorBs :: B.ByteString -> Connection -> IO Response
updateAuthorBs = updateModel . decodeAuthor
