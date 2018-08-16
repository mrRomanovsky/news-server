{-# LANGUAGE OverloadedStrings #-}

module Blog.Exceptions.Exceptions
  ( handleRequestException
  , notFound
  ) where

import Control.Exception
import Network.HTTP.Types (status404, status500)
import Network.Wai

handleRequestException :: SomeException -> IO Response
handleRequestException e = do
  appendFile "news-server.log" $
    "\nException occured during request processing: " ++ show e
  return errorOccured

errorOccured :: Response
errorOccured =
  responseLBS
    status500
    [("Content-Type", "application/json")]
    "Error occured. Please, try again later"

notFound :: Response
notFound =
  responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"
