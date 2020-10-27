module Overflow
( Host (..)
, sendPattern
) where

import Data.Text (Text, unpack)
import Data.Char (chr)

-- |...
data Host =
    Host Text Int
    deriving (Show)

-- |...
sendPattern :: Host -> Int -> (Maybe Text, Maybe Text) -> IO () 
sendPattern (Host ip port) l (p, s) = putStrLn payload
    where
        payload = createPayload (cyclicPattern l) (p, s)

-- ...
createPayload :: String -> (Maybe Text, Maybe Text) -> String
createPayload x (Just p, Just s)   = unpack p ++ x ++ unpack s
createPayload x (Just p, Nothing)  = unpack p ++ x
createPayload x (Nothing, Just s)  = x ++ unpack s
createPayload x (Nothing, Nothing) = x

-- ...
cyclicPattern :: Int -> String
cyclicPattern i = take i $ generate
    where
        generate         = concat $ map squash $ iterate nextCycle (65, 97, 48)
        squash (x, y, z) = [chr x, chr y, chr z] 

-- ...
nextCycle :: (Int, Int, Int) -> (Int, Int, Int)
nextCycle (x, y, z) = next (x, y, z + 1)
    where
        next  (a, b, 58) = next (a, b + 1, 48)
        next (a, 123, c) = next (a + 1, 97, c)
        next  (91, _, _) = next (65, 97, 48)
        next   (a, b, c) = (a, b, c)
