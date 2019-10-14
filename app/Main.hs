{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Monoid (mconcat)
import           Web.Scotty

import           Scraper


main :: IO ()
main = scotty 8080 $
  get "/" $ do
    url <- param "url"
    html url
