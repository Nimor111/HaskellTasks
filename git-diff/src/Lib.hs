{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getDiff, readBadWords, search, writeReport
    ) where

import           Control.Monad      (when)
import           Data.Text          as T
import           Data.Text.IO       as TI hiding (writeFile)
import           Html               (makeReport)
import           Prelude
import           System.Environment (getArgs)
import           System.Process     (cwd, proc, readCreateProcess)

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- Without Blaze
-- writeWordToFile :: FilePath -> T.Text -> IO()
-- writeWordToFile = TI.writeFile

-- search :: T.Text -> [T.Text] -> [T.Text]
-- search diff = mapM_ (\x -> when (x `T.isInfixOf` diff) $ writeWordToFile "report.html" x)
-- Without Blaze

writeReport :: [T.Text] -> IO()
writeReport wordsList = writeFile "report.html" $ makeReport wordsList

search :: T.Text -> [T.Text] -> [T.Text]
search diff [] = []
search diff (word:text)
  | word `T.isInfixOf` diff = word : search diff text
  | otherwise = search diff text

getDiff :: IO T.Text
getDiff = do
  args <- getArgs
  output <- readCreateProcess ((proc "git" ["diff", "HEAD", "HEAD~1"]) {cwd=maybeHead args}) ""
  return $ T.pack output

readBadWords :: FilePath -> IO [T.Text]
readBadWords file = do
  f <- TI.readFile file
  return $ T.splitOn ", " f
