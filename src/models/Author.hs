module Author where

import User
import qualified Data.ByteString.Lazy as B

data Author = Author {user :: User, desc :: B.ByteString}

{-
Авторы
Ссылка на пользователя (то есть все авторы — пользователи, но не все пользователи — авторы)
Краткое описание
-}