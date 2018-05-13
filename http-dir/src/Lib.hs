{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( crawl
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

{--
  Type to represent a url written to the JSON file.
  Consists of a link.
--}
newtype Url = Url {
      link   :: Text
    } deriving (Generic, Show)


{--
  Define default instances for serialization and deserialization since we've derived Generic.
--}
instance ToJSON Url

instance FromJSON Url

{--
  Safe alternative to the (!!) operator.
--}
getItem :: [a] -> Int -> Maybe a
getItem [] _     = Nothing
getItem (x:_) 0  = Just x
getItem (_:xs) l = getItem xs (l - 1)

{--
  Turn a list of Text to a JSON encoded list.
--}
urlToJSON :: [Text] -> ByteString
urlToJSON urlList = encode $ map (\x -> Url { link = x }) urlList

{--
  Write a list of Text ot a JSON file.
  If no file supplied, displays message.
--}
writeToFile :: Maybe FilePath -> [Text] -> IO()
writeToFile file lines =  case file of
                            Nothing -> putStrLn "No file found!"
                            Just f  -> B.appendFile  f (urlToJSON lines)

{--
  Safe alternative to the head function.
--}
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

{--
  Crawl dir.bg/search with given query as command line argument.
  If no query supplied, displays message.
--}
crawl :: IO()
crawl = do
  args <- getArgs
  case safeHead args of
    Nothing -> putStrLn "Usage: cmd searchQuery"
    Just query -> do
      doc <- simpleHttp $ "https://dir.bg/search?q=" ++ head args
      let cursor = fromDocument $ parseLBS doc
      writeToFile (getItem args 1) (cursor $// getTenAnchors &| extractData)

{--
  Return a Text containing the content of an HTML anchor href attribute.
--}
extractData :: Cursor -> Text
extractData = concat . attribute "href"

{--
  Get first ten anchors in `text-news list-article   ` class of the cursor content.
--}
getTenAnchors :: Cursor -> [Cursor]
getTenAnchors cursor = take 10 anchors
  where
    anchors = cursor $/ attributeIs "class" "text-news list-article   " &/ element "a"
