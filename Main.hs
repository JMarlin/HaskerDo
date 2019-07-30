module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "8080"
    E.bracket (open addr) close loop

resolve port = do
    let hints = defaultHints {
        addrFlags = [AI_PASSIVE],
        addrSocketType = Stream
    }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    return addr

open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    fd <- fdSocket sock
    setCloseOnExecIfNeeded fd
    bind sock (addrAddress addr)
    listen sock 10
    return sock

loop sock = forever $ do
    (conn,peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    void $ forkFinally (talk conn) (\_ -> close conn)

talk conn = do
    let drain_client = do
            msg <- recv conn 1024
            unless (S.null msg) $ do
                drain_client
    --drain_client
    print "Sending data\n"
    sendAll conn content 
    print "Sent\n"

content :: S.ByteString
content = LB.toStrict $ LBU.fromString "HTTP/1.1 200 OK\nDate: Fri, Jan 14 2019 15:45:00 EST\nContent-Type: text/html; charset=UTF-8\nContent-Length:143\nLast-Modified: Fri, Jan 14 2019 15:44:00 EST\nServer: HaskerDo/1.0\nAccept-Ranges: bytes\nConnection: close\n\n<html>\n\t<head>\n\t\t<title>An Example Page</title>\n\t</head>\n\t<body>\n\t\t<p>Hello World, this is a very simple HTML document.</p>\n\t\t</body>\n\t</html>\n"
        -- talk conn
readRequestHeader conn 
