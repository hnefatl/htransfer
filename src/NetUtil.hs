module NetUtil
(
    runServer
) where

import Network.Socket hiding (send, recv)
import Control.Monad.Catch (catchAll, throwM)
import Control.Monad.Except

import Shared

runServer :: Resources -> ExceptT String IO ()
runServer resources = do
    (listener, addr) <- getServerSocket 34652
    undefined


getServerSocket :: Int -> ExceptT String IO (Socket, AddrInfo)
getServerSocket service = catchAll (lift $ makeServerSocket service) (\e -> throwError $ show e)

makeServerSocket :: Int -> IO (Socket, AddrInfo)
makeServerSocket service = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE, AI_NUMERICSERV], addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) Nothing (Just $ show service)
    when (null addrs) (throwM "No suitable interfaces found")
    let addr = head addrs
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1 -- Make socket quickly reusable again
    bind sock (addrAddress addr)
    listen sock 10 -- Service up to 10 clients
    return (sock, addr)