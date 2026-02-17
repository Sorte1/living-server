{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dashboard.Swank
  ( SwankConnection
  , connectSwank
  , disconnectSwank
  , swankEval
  , isConnected
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (try, SomeException, catch, bracket_)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

import Dashboard.SwankProtocol

data SwankConnection = SwankConnection
  { swankSocket  :: TVar (Maybe Socket)
  , swankCounter :: TVar Int
  , swankLock    :: MVar ()  -- mutex: only one eval at a time
  }

-- | Connect to a Swank server at the given host and port.
connectSwank :: String -> Int -> IO (Either Text SwankConnection)
connectSwank host port = do
  result <- try $ do
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
    case addrs of
      [] -> error "No address found"
      (addr:_) -> do
        sock <- openSocket addr
        connect sock (addrAddress addr)
        -- Swank doesn't send a greeting — client must initiate.
        -- Send swank:connection-info to establish the session.
        let initMsg = makeConnectionInfoForm 1
        sendAll sock (encodeMessage initMsg)
        -- Read the connection-info response with a timeout
        mResponse <- timeout 10000000 $ waitForReturn sock BS.empty  -- 10 seconds
        case mResponse of
          Nothing -> do
            close sock
            error "Timeout waiting for Swank connection-info response"
          Just _ -> do
            socketVar <- newTVarIO (Just sock)
            counterVar <- newTVarIO 2  -- counter starts at 2 since we used 1
            lock <- newMVar ()
            return $ SwankConnection socketVar counterVar lock
  case result of
    Left (e :: SomeException) ->
      return $ Left $ "Failed to connect to Swank: " <> T.pack (show e)
    Right conn ->
      return $ Right conn

-- | Disconnect from the Swank server.
disconnectSwank :: SwankConnection -> IO ()
disconnectSwank conn = do
  mSock <- atomically $ do
    s <- readTVar (swankSocket conn)
    writeTVar (swankSocket conn) Nothing
    return s
  case mSock of
    Just sock -> close sock `catch` (\(_ :: SomeException) -> return ())
    Nothing   -> return ()

-- | Check if connected.
isConnected :: SwankConnection -> IO Bool
isConnected conn = do
  mSock <- readTVarIO (swankSocket conn)
  return $ case mSock of
    Just _  -> True
    Nothing -> False

-- | Evaluate a Lisp expression via Swank and return the result.
-- Uses a lock to ensure only one eval runs at a time, preventing
-- response mixups when multiple callers share the connection.
-- Times out after 120 seconds to prevent hanging forever.
swankEval :: SwankConnection -> Text -> IO (Either Text Text)
swankEval conn code = do
  mSock <- readTVarIO (swankSocket conn)
  case mSock of
    Nothing -> return $ Left "Not connected to Swank"
    Just sock ->
      bracket_ (takeMVar (swankLock conn)) (putMVar (swankLock conn) ()) $ do
        counter <- atomically $ do
          c <- readTVar (swankCounter conn)
          writeTVar (swankCounter conn) (c + 1)
          return c
        let msg = makeEvalForm code counter
        result <- try $ do
          sendAll sock (encodeMessage msg)
          -- 5 minute timeout for long operations like ql:quickload
          mResp <- timeout 300000000 $ waitForReturn sock BS.empty
          case mResp of
            Nothing -> error "Swank eval timed out after 120 seconds"
            Just response -> return response
        case result of
          Left (e :: SomeException) -> do
            atomically $ writeTVar (swankSocket conn) Nothing
            return $ Left $ "Swank eval error: " <> T.pack (show e)
          Right response ->
            case parseEvalResult response of
              Right (output, value) ->
                let combined = if T.null output then value
                              else output <> "\n" <> value
                in return $ Right combined
              Left err -> return $ Left err

-- | Read messages from the socket until we get a :return message.
-- Properly buffers partial data across recv calls so large messages
-- (e.g. from ql:quickload compilation output) aren't dropped.
waitForReturn :: Socket -> BS.ByteString -> IO Text
waitForReturn sock buffer = do
  -- Read more data from socket
  chunk <- recv sock 65536
  if BS.null chunk
    then error "Swank connection closed"
    else do
      let allData = buffer <> chunk
          (msgs, remaining) = decodeAllMessages allData
      case filter (T.isInfixOf ":return") msgs of
        (m:_) -> return m
        []    -> waitForReturn sock remaining

-- | Decode all complete messages from a buffer, returning decoded messages
-- and any remaining (partial) bytes.
decodeAllMessages :: BS.ByteString -> ([Text], BS.ByteString)
decodeAllMessages bs
  | BS.null bs = ([], bs)
  | otherwise = case decodeMessage bs of
      Just (msg, rest) ->
        let (more, remaining) = decodeAllMessages rest
        in (msg : more, remaining)
      Nothing -> ([], bs)  -- partial message, keep bytes for next read
