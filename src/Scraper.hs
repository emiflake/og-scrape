{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Scraper
       ( getProperties
       , ScrapeError(..)
       , ErrorResponse(..)
       , errorResponse
       ) where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe

import qualified Data.ByteString.Lazy   as BS
import qualified Data.Text.Lazy         as T

import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree

import           Text.Pretty.Simple

import           OpenGraph

import           Data.Aeson
import qualified Data.Map               as M

import           GHC.Generics

-- | An error response type, useful for returning errors as JSON
data ErrorResponse e
    = MkErrorResponse
    { error       :: e
    , explanation :: T.Text
    }
    deriving (Show, Eq, Generic)

-- | errorResponse helper function
errorResponse :: ToJSON e => e -> T.Text -> ErrorResponse e
errorResponse = MkErrorResponse

instance ToJSON e => ToJSON (ErrorResponse e) where

-- | An error while scraping
data ScrapeError
    = NoMetaData
    | NoHeadTag
    | RequestFailed
    deriving (Show, Eq, Generic)

instance ToJSON ScrapeError where



forceStripPrefix :: T.Text -> T.Text -> Maybe T.Text
forceStripPrefix p t
    | T.isPrefixOf p t = Just $ T.drop (T.length p) t
    | otherwise = Nothing

findBranch :: T.Text -> [TagTree T.Text] -> Maybe [TagTree T.Text]
findBranch tagName =
    fmap (\(TagBranch n _ children) -> children) .
    find (\case
        TagBranch name _ children -> name == tagName
        _                         -> False)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust _ (Just b) = Just b
firstJust _ _        = Nothing

findHead :: [TagTree T.Text] -> Maybe [TagTree T.Text]
findHead xs =
    -- This may seem weird, but TagSoup seemed to be unable to handle ogp.me
    -- by doing html > head, so this [head by itself] is a workaround
    firstJust
        (findBranch "html" xs >>= findBranch "head")
        (findBranch "head" xs)

propertyFromTag :: Tag T.Text -> Maybe OGPair
propertyFromTag (TagOpen "meta" attributes) =
    MkOGPair <$> property <*> content

    where property = lookup "property" attributes >>= forceStripPrefix "og:"
          content  = lookup "content" attributes

propertyFromTag _ = Nothing

getProperties :: T.Text -> Maybe [OGPair]
getProperties =
    fmap (mapMaybe propertyFromTag . flattenTree)
    . findHead
    . parseTree

