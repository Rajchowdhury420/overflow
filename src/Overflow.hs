module Overflow
( Host (..)
, createPayload
) where

import Data.Text (Text, unpack)

-- |...
data Host =
    Host Text Int
    deriving (Show)

-- ...
createPayload :: String -> (Maybe Text, Maybe Text) -> String
createPayload x (Just p, Just s)   = unpack p ++ x ++ unpack s
createPayload x (Just p, Nothing)  = unpack p ++ x
createPayload x (Nothing, Just s)  = x ++ unpack s
createPayload x (Nothing, Nothing) = x
