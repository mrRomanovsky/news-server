{-# LANGUAGE TypeSynonymInstances #-}

module Tag where

import Model
import qualified Data.ByteString.Lazy as B

type Tag = B.ByteString

instance Model Tag where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()