{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import qualified Data.Text.Lazy    as T
import           Text.RawString.QQ

import           Test.Hspec

import           OpenGraph
import           Scraper

justMetas :: T.Text
justMetas = T.pack [r|
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
|]

main :: IO ()
main =
    hspec $
        describe "Just Metas." $ do
            let props = getProperties justMetas
            it "should have the exact 7 props" $
                all ((`elem` props) . uncurry MkOGProperty)
                    [ ("type", "website")
                    , ("title", "Nederlandse Omroep Stichting")
                    , ("description", "NOS.nl - Nieuws, Sport en Evenementen op Radio, TV en Internet")
                    , ("image", "https://nos.nl/img/social/nos.jpg?1910141638")
                    , ("image:width", "1200")
                    , ("image:height", "630")
                    , ("url", "https://nos.nl/")
                    ]
