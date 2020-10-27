module Overflow.Pattern
( sendPattern
) where

import Overflow
import Data.Text (Text)
import Data.Char (chr)

-- |...
sendPattern :: Host -> Int -> (Maybe Text, Maybe Text) -> IO ()
sendPattern h l (p, s) = sendPayload h payload >>= out
    where
        payload   = createPayload (cyclicPattern l) (p, s)
        out  True = putStrLn "Pattern sent successfully."
        out False = putStrLn "Error: An error occurred connecting to target."

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
