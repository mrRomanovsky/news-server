{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
handleRequestException c e =
  let msg = show e
   in case fromException e of
        Just (x :: PatternMatchFail) -> logWarning c e
        _ -> logException c e

logException :: ServerConfig -> SomeException -> IO Response
logException c e = do
  appendFile (logFile c) $
    "\nException occured during request processing: " ++ show e
  return errorOccured

logWarning :: ServerConfig -> SomeException -> IO Response
logWarning c w = do
  when
    (logLevel c <= Warning)
    (appendFile (logFile c) $ "\nWarning (request processing) : " ++ show w)
  return notFound

{-
import Control.Exception as X

func = X.catch (print $ asd []) printErr

printErr :: SomeException -> IO ()
printErr e =  do
        case fromException e of
                Just (x:: PatternMatchFail) -> putStrLn "I caught the exception"
                                            >> print x
                nothing -> return ()

asd :: [Int] -> [Int]
asd (x:xs) = xs
-}
errorOccured :: Response
errorOccured =
  responseLBS
    status500
    [("Content-Type", "application/json")]
    "Error occured. Please, try again later"

notFound :: Response
notFound =
  responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"
