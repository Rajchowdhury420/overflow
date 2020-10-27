module Overflow
( Host (..)
, createPayload
, sendPayload
) where


import qualified Control.Exception         as E
import qualified Data.ByteString.Char8     as Char8
import           Data.Text                 (Text, unpack)
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

-- |...
data Host =
    Host Text Text
    deriving (Show)

-- |...
createPayload :: String -> (Maybe Text, Maybe Text) -> String
createPayload x (Just p, Just s)   = unpack p ++ x ++ unpack s
createPayload x (Just p, Nothing)  = unpack p ++ x
createPayload x (Nothing, Just s)  = x ++ unpack s
createPayload x (Nothing, Nothing) = x

-- |...
sendPayload :: Host -> String -> IO Bool
sendPayload (Host a p) x = withSocketsDo $ do
        addr <- resolve
        result <- E.try (run addr) :: IO (Either E.SomeException Bool)
        out result
    where
        resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just (unpack a)) (Just (unpack p))
        run addr = do
            sock <- openSocket addr
            connect sock $ addrAddress addr
            sendAll sock (Char8.pack x)
            _ <- recv sock 1024
            pure True
        out (Left _)  = pure False
        out (Right _) = pure True
