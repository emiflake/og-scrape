{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class



import           Data.Aeson
import           Web.Scotty

import           OpenGraph
import           Scraper


main :: IO ()
main = scotty 8080 $
  get "/" $ do
    url <- param "url"
    htmlContent <- liftIO $ openURL url
    Web.Scotty.json . encodeProperties $ getProperties htmlContent
