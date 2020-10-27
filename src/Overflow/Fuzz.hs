module Overflow.Fuzz
( runFuzzer
) where

import Overflow
import Control.Concurrent
import Data.Text          (Text)
import Text.Printf

-- |...
runFuzzer :: Host -> Int -> (Maybe Text, Maybe Text) -> IO ()
runFuzzer h i x = runFuzzer' h i i x True >>= out 
    where
        out c
            |    c == i = putStrLn "Error: An error occurred sending payload to target."
            | otherwise = printf "Done! Length of buffer is in the range [%d, %d].\n" (c - i) c

-- ...
runFuzzer' :: Host -> Int -> Int -> (Maybe Text, Maybe Text) -> Bool -> IO Int
runFuzzer' _ c i _ False = pure (c - i)
runFuzzer'  h c i x True = do
        result <- sendPayload h payload 
        printf "    ───> Sending %d-byte payload to target...\n" c
        threadDelay 1000000 -- 1s
        runFuzzer' h (c + i) i x result
    where
        payload = createPayload (replicate c 'A') x
