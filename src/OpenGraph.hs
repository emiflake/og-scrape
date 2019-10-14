module OpenGraph where

import qualified Data.Text.Lazy as T

data OGProperty =
    MkOGProperty { name  :: T.Text
                 , value :: T.Text }
                 deriving Show


