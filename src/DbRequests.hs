{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DbRequests where
import Model
import Data.String (fromString)
import Network.Wai
import Network.HTTP.Types (status200, status404, hAuthorization, Query)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types hiding (Query)
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Applicative
import Data.Text

type AuthData = B.ByteString
{-
type QueryGet = forall m id. Model m id=> Query -> Connection -> IO [m]

paginate :: Model m id => Query -> Maybe Integer -> QueryGet -> Connection -> IO [m]
paginate q Nothing qGet = qGet $
  read $ show q ++ " LIMIT 20"
paginate q (Just page) qGet = qGet $
  read $ show q ++ " OFFSET " ++ show page
      ++ " LIMIT 20" 
-}
getDraftAuthor :: Integer -> Connection -> IO Integer
getDraftAuthor dId conn = do
  authorId <- query conn "SELECT author_id FROM drafts WHERE draft_id = ?"
    [dId]
  case authorId of
    [[aId]] -> return aId
    _       -> error "draft didn't have an author!"


getAuthorId :: B.ByteString -> Connection -> IO (Maybe Integer)
getAuthorId uId conn = do
  authorId <- query conn "SELECT author_id FROM authors WHERE users_id = ?"
    [uId]
  case authorId of
    [[aId]] -> return $ Just aId
    _       -> return Nothing

authResponse :: Maybe AuthData -> (Connection -> IO Response) -> Connection -> IO Response
authResponse auth respond conn = do
  isAdmin <- authorizeAdmin auth conn
  if isAdmin
     then respond conn
     else return notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

authorizeAdmin :: Maybe AuthData -> Connection -> IO Bool
authorizeAdmin auth conn = 
  maybe (return False) (`checkAdmin` conn) auth

checkAdmin :: B.ByteString -> Connection -> IO Bool
checkAdmin uId conn = do
  adminUser <- query conn "SELECT is_admin FROM users WHERE users_id = ?"
    [uId]
  case adminUser  of
    [[True]] -> return True
    _      -> return False

getRecords :: FromRow m => Text -> Maybe Integer -> Maybe B.ByteString -> Connection -> IO [m]
getRecords table page sortParam conn = 
  let defSort = if Data.Text.head table == 'u'
                   then table `append` "_id"
                   else Data.Text.init table `append` "_id"
      sortP = Identifier $ maybe defSort decodeUtf8 sortParam
      in query conn (paginate selectOrdered page) (Identifier table, sortP)
{-getRecords table Nothing conn =
  query conn "SELECT * FROM ? LIMIT 20" [Identifier table]
getRecords table (Just page) conn =
  let offset = (page - 1) * 20
      in query conn "SELECT * FROM ? OFFSET ? LIMIT 20" (Identifier table, offset)-}

selectOrdered :: Database.PostgreSQL.Simple.Query
selectOrdered = "SELECT * FROM ? ORDER BY ?"

paginate :: Database.PostgreSQL.Simple.Query -> Maybe Page -> Database.PostgreSQL.Simple.Query
paginate q p =
  let offset = maybe 0 ((*20) . (subtract 1)) p
      qStr = Prelude.init $ Prelude.tail $ show q
      in fromString $ qStr ++
        "OFFSET " ++ show offset ++ " LIMIT 20"