{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Maybe

import qualified Data.Map.Strict   as Map
import qualified Data.Text.Lazy    as T
import           Text.RawString.QQ

import           Test.Hspec

import           OpenGraph
import           Scraper

justMetas :: T.Text
justMetas = T.pack [r|
<html>
    <head>
        <meta property="og:type" content="website" />
        <meta property="og:title" content="Nederlandse Omroep Stichting" />
        <meta property="og:description" content="NOS.nl - Nieuws, Sport en Evenementen op Radio, TV en Internet" />
        <meta property="og:image" content="https://nos.nl/img/social/nos.jpg?1910141638" />
        <meta property="og:image:width" content="1200" />
        <meta property="og:image:height" content="630" />
        <meta property="og:url" content="https://nos.nl/" />

        <meta property="twitter:card" content="app" />
        <meta property="twitter:site" content="@NOS" />
        <meta property="twitter:app:id:iphone" content="id516491461" />
        <meta property="twitter:app:url:iphone" content="nl.nos.app://open" />
        <meta property="twitter:app:id:ipad" content="id516491461" />
        <meta property="twitter:app:url:ipad" content="nl.nos.app://open" />
        <meta property="twitter:app:id:googleplay" content="nl.nos.app" />
        <meta property="twitter:app:country" content="nl" />
    </head>
</html>
|]

arrays :: T.Text
arrays = T.pack [r|
<html>
    <head>
        <meta property="og:foobars" content="foo" />
        <meta property="og:foobars" content="bar" />
        <meta property="og:foobars" content="baz" />
        <meta property="og:foobars" content="qux" />
    </head>
</html>
|]

imageExample :: T.Text
imageExample = T.pack [r|
<html>
    <head>
        <meta property="og:title" content="My Cute Cat Image" />
        <meta property="og:image" content="https://cdn.pixabay.com/photo/2017/02/20/18/03/cat-2083492_960_720.jpg" />
        <meta property="og:image:width" content="960" />
        <meta property="og:image:height" content="585" />
        <meta property="og:image:type" content="JPEG" />
        <meta property="og:image:tags" content="cat" />
        <meta property="og:image:tags" content="animal" />
        <meta property="og:image:tags" content="cute" />
    </head>
</html>
|]

main :: IO ()
main =
    hspec $ do
        describe "Array object tree building" $ do
            let props = fromJust $ getProperties arrays
            it "should have 4 props" $
                length props `shouldBe` 4
            it "should equate to an array" $
                -- It does reverse the order, but it's a dictionary, so it doesn't matter.
                treeFromPairs props `shouldBe`
                    Branch (Map.fromList [ ("foobars", List [ Value "qux" -- qux is last mentioned in src,
                                                            , Value "baz" --  but it's the first in this list.
                                                            , Value "bar"
                                                            , Value "foo"
                                                            ]
                                         ) ] )
        describe "Complicated subobject building" $ do
            let props = fromJust $ getProperties imageExample
            it "should have 8 props" $
                length props `shouldBe` 8
            it "should build up complex structure" $
                treeFromPairs props `shouldBe`
                    Branch (Map.fromList
                        [ ( "title", Value "My Cute Cat Image" )
                        , ( "image", Branch (Map.fromList
                            [ ( "content" , Value "https://cdn.pixabay.com/photo/2017/02/20/18/03/cat-2083492_960_720.jpg")
                            , ( "width", Value "960")
                            , ( "height", Value "585")
                            , ( "type", Value "JPEG")
                            , ( "tags", List
                                [ Value "cute"
                                , Value "animal"
                                , Value "cat"
                                ])
                            ] ))
                        ] )


        describe "Simple metas, with dummy data" $ do
            let props = fromJust $ getProperties justMetas
            it "should only have 7 props" $
                length props `shouldBe` 7
            it "should have the 7 props" $
                all ((`elem` props) . uncurry MkOGPair)
                    [ ("type", "website")
                    , ("title", "Nederlandse Omroep Stichting")
                    , ("description", "NOS.nl - Nieuws, Sport en Evenementen op Radio, TV en Internet")
                    , ("image", "https://nos.nl/img/social/nos.jpg?1910141638")
                    , ("image:width", "1200")
                    , ("image:height", "630")
                    , ("url", "https://nos.nl/")
                    ]
