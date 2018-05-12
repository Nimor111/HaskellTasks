module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as TI
import           Lib

main :: IO ()
main = do
  diff <- getDiff
  badWords <- readBadWords "cpp_bad_words"
  search diff badWords
