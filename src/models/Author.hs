module Author where

import User
import Model
import qualified Data.ByteString.Lazy as B

data Author = Author {user :: User, desc :: B.ByteString}

instance Model Author where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-
Авторы
Ссылка на пользователя (то есть все авторы — пользователи, но не все пользователи — авторы)
Краткое описание
-}