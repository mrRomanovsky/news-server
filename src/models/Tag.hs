{-# LANGUAGE TypeSynonymInstances #-}

module Tag where

import Model
import Data.Text

type Tag = Text

instance Model Tag where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()