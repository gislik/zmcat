
module Network.Zmcat where

import Prelude hiding (getLine, drop, length)
import System.ZMQ3
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import System.IO (stdout, hFlush, isEOF)

runCtx :: SocketType a1 => a1 -> (Socket a1 -> IO a) -> IO a
runCtx t blk = withContext $ \ctx -> do
        withSocket ctx t $ \skt -> do
            blk skt

pub :: String -> String -> IO a
pub uri k = runCtx Pub $ \skt -> do
        bind skt uri
        forever $ do
            pkt <- getLine 
            send skt [] (C.pack k <> pkt)

sub :: String -> ByteString -> IO a
sub uri k = runCtx Sub $ \skt -> do
        subscribe skt k
        connect skt uri
        forever $ do
            line <- receive skt
            C.putStrLn $ drop (length k) line
            hFlush stdout
                
pull :: String -> IO a
pull uri = runCtx Pull $ \skt -> do
        bind skt uri
        forever $ do
            line <- receive skt
            C.putStrLn line
            hFlush stdout

push :: String -> IO a
push uri = runCtx Push $ \skt -> do
        connect skt uri
        forever $ do
            pkt <- getLine 
            eof <- isEOF
            if eof
               then send skt [] pkt
               else send skt [SendMore] pkt

rep :: String -> IO a
rep uri = runCtx Rep $ \skt -> do
    bind skt uri
    forever $ do
        line <- receive skt
        C.putStrLn line
        hFlush stdout
        pkt <- getLine 
        send skt [] pkt

req :: String -> IO a
req uri = runCtx Req $ \skt -> do
    connect skt uri
    forever $ do
        pkt <- getLine 
        send skt [] pkt
        lines' <- receiveMulti skt
        mapM_ C.putStrLn lines'
        hFlush stdout
