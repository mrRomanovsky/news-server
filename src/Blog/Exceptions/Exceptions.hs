{-# LANGUAGE OverloadedStrings #-}

module Blog.Exceptions.Exceptions
  ( handleRequestException
  ) where

import Control.Exception
import Network.HTTP.Types (status500)
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
