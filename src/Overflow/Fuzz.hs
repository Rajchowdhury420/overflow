module Overflow.Fuzz
( runFuzzer
) where

import Overflow
import Control.Concurrent
import Text.Printf

-- |...
runFuzzer :: Host -> Int -> Affix -> IO ()
runFuzzer h i a = runFuzzer' h i i a True >>= printResult i 

-- ...
runFuzzer' :: Host -> Int -> Int -> Affix -> Bool -> IO Int
runFuzzer' _ c i _ False = pure (c - i)
runFuzzer'  h c i a True = do
        printf "    ───> Sending %d-byte payload to target...\n" c
        result <- sendPayload h payload 
        threadDelay 1000000 -- 1s
        runFuzzer' h (c + i) i a result
    where
        payload = createPayload (replicate c 'A') a

-- ...
printResult :: Int -> Int -> IO () 
printResult i c
    |    c == i = putStrLn "Error: An error occurred sending payload to target."
    | otherwise = printf "Done! Length of buffer is in the range (%d, %d].\n" (c - i) c
