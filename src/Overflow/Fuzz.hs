module Overflow.Fuzz
( runFuzzer
) where

import Overflow
import Control.Concurrent

runFuzzer :: Host -> Int -> IO ()
runFuzzer = do 

sendPayload :: Host -> String -> IO ()
sendPayload = do
    threadDelay 500000 -- 0.5s
