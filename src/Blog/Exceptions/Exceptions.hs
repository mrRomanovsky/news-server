{-# LANGUAGE OverloadedStrings #-}

module Blog.Exceptions.Exceptions
  ( handleRequestException
  , notFound
  ) where

import Blog.Config.Config
import Blog.Config.Logging
import Control.Exception
import Control.Monad (when)
import Network.HTTP.Types (status404, status500)
import Network.Wai

handleRequestException :: ServerConfig -> SomeException -> IO Response
handleRequestException c e = do
  when
    (logLevel c >= Debug)
    (appendFile (logFile c) $
     "\nException occured during request processing: " ++ show e)
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
