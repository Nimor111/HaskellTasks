{-# LANGUAGE OverloadedStrings #-}

module Html(
  makeReport
) where

import           Control.Monad                   (forM_)

import           Data.Text                       as T
import           Text.Blaze.Html.Renderer.Pretty as R
import           Text.Blaze.Html5                as H

makeReport :: [T.Text] -> String
makeReport wordsList = renderHtml $ docTypeHtml $ do
  H.head $ do
    H.title "Report"
  body $ do
    h1 "Bad words in diff"
    forM_ wordsList (p . toHtml)
