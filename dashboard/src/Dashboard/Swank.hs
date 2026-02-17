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
import Data.Char (isDigit)
import Data.IORef
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
        let initMsg = makeConnectionInfoForm 1
        sendAll sock (encodeMessage initMsg)
        counterVar <- newTVarIO 2
        debugRef <- newIORef Nothing
        mResponse <- timeout 10000000 $
          waitForReturn sock BS.empty counterVar debugRef
        case mResponse of
          Nothing -> do
            close sock
            error "Timeout waiting for Swank connection-info response"
          Just _ -> do
            socketVar <- newTVarIO (Just sock)
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
        -- Track debug conditions so we can report real errors
        debugRef <- newIORef Nothing
        result <- try $ do
          sendAll sock (encodeMessage msg)
          mResp <- timeout 300000000 $
            waitForReturn sock BS.empty (swankCounter conn) debugRef
          case mResp of
            Nothing -> error "Swank eval timed out after 5 minutes"
            Just response -> return response
        case result of
          Left (e :: SomeException) -> do
            close sock `catch` (\(_ :: SomeException) -> return ())
            atomically $ writeTVar (swankSocket conn) Nothing
            return $ Left $ "Swank eval error: " <> T.pack (show e)
          Right response ->
            case parseEvalResult response of
              Right (output, value) ->
                let combined = if T.null output then value
                              else output <> "\n" <> value
                in return $ Right combined
              Left err -> do
                -- If we have a captured debug condition, include it
                mDebug <- readIORef debugRef
                case mDebug of
                  Just debugErr -> return $ Left $ debugErr
                  Nothing -> return $ Left err

-- | Read messages from the socket until we get a :return message.
-- Captures error info from :debug messages and auto-aborts from the debugger.
waitForReturn :: Socket -> BS.ByteString -> TVar Int -> IORef (Maybe Text) -> IO Text
waitForReturn sock buffer counterVar debugRef = do
  chunk <- recv sock 65536
  if BS.null chunk
    then error "Swank connection closed"
    else do
      let allData = buffer <> chunk
          (msgs, remaining) = decodeAllMessages allData
      -- Capture error text from :debug messages
      mapM_ (captureDebugCondition debugRef) msgs
      -- Auto-abort from debugger
      mapM_ (handleDebugMessage sock counterVar) msgs
      case filter (T.isInfixOf ":return") msgs of
        (m:_) -> return m
        []    -> waitForReturn sock remaining counterVar debugRef

-- | Extract the error condition text from a :debug message.
-- Format: (:debug <thread> <level> (<condition-string> <type> ...) ...)
-- The condition string is the first quoted string after the level number.
captureDebugCondition :: IORef (Maybe Text) -> Text -> IO ()
captureDebugCondition debugRef msg
  | ":debug " `T.isInfixOf` msg
  , not (":debug-activate" `T.isInfixOf` msg) =
      let strings = extractQuotedStrings msg
      in case strings of
           (condText:_) | not (T.null condText) ->
             writeIORef debugRef (Just condText)
           _ -> return ()
  | otherwise = return ()

-- | When Swank enters the debugger, automatically invoke restart 0
-- so the eval doesn't hang waiting for user input.
handleDebugMessage :: Socket -> TVar Int -> Text -> IO ()
handleDebugMessage sock counterVar msg
  | ":debug-activate" `T.isInfixOf` msg = do
      let (thread, level) = parseDebugActivate msg
      counter <- atomically $ do
        c <- readTVar counterVar
        writeTVar counterVar (c + 1)
        return c
      let abortMsg = "(:emacs-rex (swank:invoke-nth-restart-for-emacs "
                     <> T.pack (show level) <> " 0) \"LIVING-SERVER\" "
                     <> T.pack (show thread) <> " "
                     <> T.pack (show counter) <> ")"
      sendAll sock (encodeMessage abortMsg)
  | otherwise = return ()

parseDebugActivate :: Text -> (Int, Int)
parseDebugActivate msg =
  let nums = extractNumbers (T.unpack msg)
  in case nums of
       (t:l:_) -> (t, l)
       [t]     -> (t, 1)
       []      -> (1, 1)

extractNumbers :: String -> [Int]
extractNumbers s =
  let afterTag = case breakOnSubstring "debug-activate" s of
                   Just rest -> rest
                   Nothing   -> s
  in extractNums afterTag
  where
    extractNums [] = []
    extractNums cs =
      let (digits, rest) = span isDigit (dropWhile (not . isDigit) cs)
      in if null digits then [] else read digits : extractNums rest

    breakOnSubstring _ [] = Nothing
    breakOnSubstring sub str@(_:rest)
      | sub `isPrefixOfStr` str = Just (drop (length sub) str)
      | otherwise = breakOnSubstring sub rest

    isPrefixOfStr [] _ = True
    isPrefixOfStr _ [] = False
    isPrefixOfStr (a:as') (b:bs) = a == b && isPrefixOfStr as' bs

decodeAllMessages :: BS.ByteString -> ([Text], BS.ByteString)
decodeAllMessages bs
  | BS.null bs = ([], bs)
  | otherwise = case decodeMessage bs of
      Just (msg, rest) ->
        let (more, remaining) = decodeAllMessages rest
        in (msg : more, remaining)
      Nothing -> ([], bs)
