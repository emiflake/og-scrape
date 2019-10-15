{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenGraph
    ( OGTree(..)
    , OGPair(..)
    , OGDict
    , treeFromPairs
    ) where

import           Data.Aeson
import qualified Data.Map.Strict as Map

import           Debug.Trace

import qualified Data.Text.Lazy  as T


data OGPair =
    MkOGPair { name  :: T.Text
             , value :: T.Text
             }
             deriving (Show, Eq)

type OGDict = (Map.Map T.Text OGTree)

data OGTree
    = Branch OGDict
    | List [OGTree]
    | Value T.Text
    deriving (Show, Eq)

instance ToJSON OGTree where
    toJSON (Branch d) = toJSON d
    toJSON (List xs)  = toJSON xs
    toJSON (Value v)  = toJSON v


-- | Merge two values into a single one, preferring lists.
mergeValues :: OGTree -- ^ What we're merging
            -> OGTree -- ^ With what we're merging
            -> OGTree
mergeValues (Value x) (List ys) = List $ Value x : ys
mergeValues (List xs) (List ys) = List $ xs ++ ys
mergeValues (Value x) (Value y) = List . map Value $ [x, y]
-- Silent failure, unable to merge, (in practice, this won't happen)
mergeValues a b                 = a

-- | Assign a pair into a dict, using merging if necessary.
assignTo :: OGPair -- ^ The pair we'll assign
         -> OGDict -- ^ The dictionary we'll asign it in
         -> OGDict
assignTo MkOGPair{name, value} = Map.insertWith mergeValues name (Value value)

-- | Merge two values by using named prefix in dictionary.
mergeDict :: T.Text -- ^ Named prefix
          -> OGTree -- ^ Inserted value
          -> OGTree -- ^ Dictionary
          -> OGTree -- ^ Result
mergeDict name (Value x) (Value y) = Branch $ Map.fromList [("content", Value y), (name, Value x)]
mergeDict name (Value y) (Branch map) = Branch $ Map.insertWith mergeValues name (Value y) map
-- See above, possible other solution: use Maybe
mergeDict _ a _ = a

assignToDict :: (T.Text, OGPair)
             -> OGDict
             -> OGDict
assignToDict (prefix, MkOGPair{name, value}) = Map.insertWith (mergeDict name) prefix (Value value)

reductor :: OGDict
         -> OGPair
         -> OGDict
reductor dict pair@MkOGPair{name, value}
    | T.any (==':') name =
        let (prefix, rest) = span (/=':') (T.unpack name)
        in assignToDict (T.pack prefix, MkOGPair (T.pack (tail rest)) value) dict
    | otherwise = assignTo pair dict


-- | Convert parsed pairs into tree structure
treeFromPairs :: [OGPair]
              -> OGTree
treeFromPairs = Branch . foldl reductor Map.empty
