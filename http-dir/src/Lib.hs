{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( testHTML, Url(..)
  ) where

import           Data.Aeson              (FromJSON, ToJSON, encode)
import           Data.ByteString.Lazy    as B hiding (concat, head, map,
                                               putStrLn, take)
import           Data.Maybe              (isNothing)
import           Data.Text               (concat)
import           Data.Text.Internal
import qualified Data.Text.IO            as T
import           GHC.Generics
import           Network.HTTP.Conduit    (simpleHttp)
import           Prelude                 hiding (concat)
import           System.Environment.UTF8 (getArgs)
import           Text.HTML.DOM           (parseLBS)
import           Text.XML.Cursor         (Cursor, attribute, attributeIs,
                                          content, element, fromDocument,
                                          hasAttribute, ($/), ($//), (&/),
                                          (&//), (&|))

newtype Url = Url {
      link   :: Text
    } deriving (Generic, Show)


instance ToJSON Url

instance FromJSON Url

getItem :: [a] -> Int -> Maybe a
getItem [] _     = Nothing
getItem (x:_) 0  = Just x
getItem (_:xs) l = getItem xs (l - 1)

urlToJSON :: [Text] -> ByteString
urlToJSON urlList = encode $ map (\x -> Url { link = x }) urlList

writeToFile :: Maybe FilePath -> [Text] -> IO()
writeToFile file lines =  case file of
                            Nothing -> putStrLn "No file found!"
                            Just f  -> B.appendFile  f (urlToJSON lines)

testHTML :: IO()
testHTML = do
  args <- getArgs
  doc <- simpleHttp $ "https://dir.bg/search?q=" ++ head args
  let cursor = fromDocument $ parseLBS doc
  writeToFile (getItem args 1) (cursor $// getTenAnchors &| extractData)

extractData :: Cursor -> Text
extractData = concat . attribute "href"

getTenAnchors :: Cursor -> [Cursor]
getTenAnchors cursor = take 10 anchors
  where
    anchors = cursor $/ attributeIs "class" "text-news list-article   " &/ element "a"
