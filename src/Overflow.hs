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
import           System.Timeout

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
        addr <- resolveAddress
        (E.try (runSocket x addr) :: IO (Either E.SomeException Bool)) >>= handleOutput
    where
        resolveAddress = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just (unpack a)) (Just (unpack p))
        handleOutput (Left _)  = pure False
        handleOutput (Right r) = pure r

-- ...
runSocket :: String -> AddrInfo -> IO Bool
runSocket x a = do
        sock <- openSocket a
        connect sock $ addrAddress a
        sendAll sock (Char8.pack x)
        timeout 5000000 (recv sock 1024) >>= handleOutput
    where
        handleOutput Nothing  = pure False
        handleOutput (Just _) = pure True

