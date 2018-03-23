module Server
(
    runServer,

    -- Reexported from Network
    HostName,
    ServiceName
) where

import Network (HostName, withSocketsDo)
import Network.Socket (ServiceName)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Concurrent.MVar
import Network.Transport
import Network.Transport.TCP
import Data.Serialize
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Shared
import Message

data ServerError = MalformedMessage String
                 | ReceivedResponse
instance Show ServerError where
    show (MalformedMessage s) = "Got a malformed message: " ++ show s
    show ReceivedResponse = "Received a response message, server can only handle request messages"
instance Exception ServerError

runServer :: HostName -> ServiceName -> Resources -> IO ()
runServer host service resources = withSocketsDo $ do
    outputLock <- newMVar ()
    transport <- unwrapEither (createTransport host service (\service' -> (host, service')) defaultTCPParameters)
    endpoint <- unwrapEither (newEndPoint transport)

    atomicPutStrLn outputLock ("Server started on: " ++ show (address endpoint))
    forever (handleEvent resources endpoint outputLock)


-- TODO - add some status messages
handleEvent :: Resources -> EndPoint -> MVar () -> IO ()
handleEvent resources endpoint outputLock = do
        event <- receive endpoint
        case event of
            -- We can ignore connections being opened/closed as we're stateless - we only act upon receiving data
            ConnectionOpened _ _ _ -> return ()
            ConnectionClosed _ -> return ()
            -- If we receive multicast data, just ignore it
            ReceivedMulticast _ _ -> return ()

            Received id payloads -> handleRequest resources id (B.concat payloads)

            EndPointClosed -> return () -- TODO - terminate a better way, some form of while loop probably
            ErrorEvent e -> throwM e

handleRequest :: Resources -> ConnectionId -> B.ByteString -> IO ()
handleRequest resources cid payload = do
    case decode payload of
        Left err -> return () -- Ignore malformed messages (TODO: add logging)
        Right message -> case message of
            ResourceListRequest -> undefined
            ResourceRequest resName -> undefined
            _ -> return () -- Only handle Request messages (TODO: add logging)

    

-- Replace with a "logger" monad? Could add like MonadLogger constraint, with `log` and `logAtomic` functions
-- and provide overrides for IO
atomicPutStrLn :: MVar () -> String -> IO ()
atomicPutStrLn l s = withMVar l (\_ -> putStrLn s)

unwrapEither :: (MonadThrow m, Exception e) => m (Either e b) -> m b
unwrapEither mx = do
                    x <- mx
                    case x of
                        Left e    -> throwM e
                        Right val -> return val