{-# LANGUAGE OverloadedStrings #-}
module Scraper where

import           Data.Char
import           Data.Maybe
import           Debug.Trace

import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T

import           Network.HTTP.Conduit
import           Text.HTML.TagSoup
import           Text.Pretty.Simple

import           OpenGraph

openURL :: String -> IO T.Text
openURL =
    fmap T.decodeUtf8 . simpleHttp

suchThat :: (a -> Bool) -> a -> Maybe a
suchThat f a | f a       = Just a
             | otherwise = Nothing

propertyFromTag :: Tag T.Text -> Maybe OGProperty
propertyFromTag (TagOpen "meta" attributes) =
    MkOGProperty <$> property <*> content

    where property = lookup "property" attributes >>= suchThat (T.isPrefixOf "og:")
          content  = lookup "content" attributes

propertyFromTag _ = Nothing

getProperties :: T.Text -> [OGProperty]
getProperties src = metaTags
    where tags = parseTags src
          metaTags = mapMaybe propertyFromTag tags

