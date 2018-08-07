module TestUtils
  ( checkResult
  , performUnitTests
  , getLocTimestamp
  ) where

import Control.Monad.Except
import qualified Database.PostgreSQL.Simple.Time as T

checkResult :: String -> Bool -> Either String Bool
checkResult testName False = throwError $ "error : " ++ testName
checkResult _ True = Right True

testResult :: (String, Either String Bool) -> String
testResult (testName, Right b) = "\t" ++ testName ++ " passed : " ++ show b
testResult (testName, Left err) = "\t" ++ testName ++ " failed : " ++ err

performUnitTests :: String -> [(String, Either String Bool)] -> String
performUnitTests moduleName tests =
  unlines $ moduleName : (testResult <$> tests)

getLocTimestamp = either undefined id . T.parseLocalTimestamp