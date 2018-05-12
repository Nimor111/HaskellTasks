{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getDiff, diffIntoLines, readBadWords, search
    ) where

import           Control.Monad      (when)
import           Data.List.Split
import           Data.Text          as T
import           Data.Text.IO       as TI
import           System.Environment (getArgs)
import           System.Process     (cwd, proc, readCreateProcess)

diffIntoLines :: IO T.Text -> IO [T.Text]
diffIntoLines diff = do
  r <- diff
  return $ T.lines r

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

writeWordToFile :: FilePath -> T.Text -> IO()
writeWordToFile = TI.writeFile

search :: T.Text -> [T.Text] -> IO()
search diff = mapM_ (\x -> when (x `T.isInfixOf` diff) $ writeWordToFile "report.html" x)

getDiff :: IO T.Text
getDiff = do
  args <- getArgs
  output <- readCreateProcess ((proc "git" ["diff", "HEAD~1", "HEAD~2"]) {cwd=maybeHead args}) ""
  return $ T.pack output

readBadWords :: FilePath -> IO [T.Text]
readBadWords file = do
  f <- TI.readFile file
  return $ T.splitOn ", " f
