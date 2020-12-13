module Util where

import Prelude (String, dropWhile)
import Data.Char (isSpace)

lstrip :: String -> String
lstrip = dropWhile isSpace
