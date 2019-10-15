{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Monad.IO.Class

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import           Network.HTTP.Conduit
import           Web.Scotty

import           OpenGraph
import           Scraper

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    scotty 8080 $ do
        defaultHandler (\e -> do
            liftIO $ print e
            Web.Scotty.json $ errorResponse RequestFailed "The website did not respond")
        get "/" $ do
            url <- param "url"
            htmlContent <- liftAndCatchIO $ do
                request <- parseRequest url
                bs <- httpLbs (request {requestHeaders = [("User-Agent","HTTP-Conduit")]}) manager
                pure . T.decodeUtf8' . responseBody $ bs
            case htmlContent of
                Left _ -> Web.Scotty.json $ errorResponse RequestFailed "The response body could not be decoded"
                Right content -> do
                    liftIO $ putStrLn $ T.unpack (mconcat ["GETTING ", T.pack url])
                    liftIO $ putStrLn $ T.unpack content
                    case getProperties content of
                        Nothing    -> Web.Scotty.json (errorResponse NoHeadTag "The website has malformed structure")
                        Just []    -> Web.Scotty.json (errorResponse NoMetaData "The website had no metadata")
                        Just props -> Web.Scotty.json $ treeFromPairs props
