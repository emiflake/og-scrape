module OpenGraph where

import           Data.Aeson
import qualified Data.Map           as M

import qualified Data.Text.Lazy     as T
import           Text.Pretty.Simple

data OGProperty =
    MkOGProperty { name  :: T.Text
                 , value :: T.Text }
                 deriving (Show, Eq)

encodeProperties :: [OGProperty] -> Value
encodeProperties = toJSON . M.fromList . map ((,) <$> name <*> value)
