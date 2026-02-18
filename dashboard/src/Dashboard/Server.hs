{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Server
  ( startDashboard
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Data.Aeson (object, (.=), decode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status (status400, status403)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty

import Dashboard.Types
import Dashboard.Claude (callClaude, extractCodeBlock, extractExplanation)
import Dashboard.Prompt (buildSystemPrompt, safetyCheck)
import Dashboard.Swank (SwankConnection, connectSwank, swankEval, isConnected)
import Dashboard.Process (LispProcess, startLispProcess, stopLispProcess, isLispRunning)
import Dashboard.Persistence (persistCode)

data AppState = AppState
  { appSwank    :: TVar (Maybe SwankConnection)
  , appProcess  :: LispProcess
  , appHistory  :: TVar [ChatMessage]
  , appBaseDir  :: FilePath
  }

-- | Start the dashboard web server on port 8080.
startDashboard :: LispProcess -> FilePath -> IO ()
startDashboard lispProc baseDir = do
  swankVar   <- newTVarIO Nothing
  historyVar <- newTVarIO []
  let state = AppState swankVar lispProc historyVar baseDir

  -- Start background Swank connector
  _ <- forkIO $ connectSwankRetry state 30

  scotty 8080 $ do
    middleware $ staticPolicy (addBase (baseDir ++ "/dashboard/static"))

    -- Serve the dashboard
    get "/" $ do
      setHeader "Content-Type" "text/html"
      file (baseDir ++ "/dashboard/static/index.html")

    -- Start the Lisp server
    post "/api/start" $ do
      result <- liftIO $ startLispProcess (appProcess state)
      case result of
        Left err -> json $ object ["success" .= False, "error" .= err]
        Right () -> do
          -- Connect Swank in background
          _ <- liftIO $ forkIO $ connectSwankRetry state 15
          json $ object ["success" .= True]

    -- Stop the Lisp server
    post "/api/stop" $ do
      liftIO $ do
        mConn <- readTVarIO (appSwank state)
        case mConn of
          Just conn -> do
            _ <- swankEval conn "(living-server:stop-all)"
            return ()
          Nothing -> return ()
        stopLispProcess (appProcess state)
        atomically $ writeTVar (appSwank state) Nothing
      json $ object ["success" .= True]

    -- Restart the Lisp server
    post "/api/restart" $ do
      liftIO $ do
        stopLispProcess (appProcess state)
        atomically $ writeTVar (appSwank state) Nothing
      result <- liftIO $ startLispProcess (appProcess state)
      case result of
        Left err -> json $ object ["success" .= False, "error" .= err]
        Right () -> do
          _ <- liftIO $ forkIO $ connectSwankRetry state 15
          json $ object ["success" .= True]

    -- Server status
    get "/api/status" $ do
      running <- liftIO $ isLispRunning (appProcess state)
      connected <- liftIO $ do
        mConn <- readTVarIO (appSwank state)
        case mConn of
          Just conn -> isConnected conn
          Nothing   -> return False
      json $ object
        [ "running"   .= running
        , "connected" .= connected
        , "httpPort"  .= (3001 :: Int)
        , "swankPort" .= (4005 :: Int)
        ]

    -- Get routes from the Lisp server
    get "/api/routes" $ do
      conn <- liftIO $ ensureSwank state
      case conn of
        Nothing -> json $ object ["routes" .= ([] :: [Route])]
        Just c -> do
          result <- liftIO $ swankEval c "(living-server:list-routes)"
          case result of
            Left _err -> json $ object ["routes" .= ([] :: [Route])]
            Right output -> do
              let routes = parseLispRoutes output
              json $ object ["routes" .= routes]

    -- Generate code from Claude (no eval yet)
    post "/api/generate" $ do
      body' <- body
      case decode body' :: Maybe GenerateRequest of
        Nothing -> do
          Web.Scotty.status status400
          json $ object ["error" .= ("Invalid request body" :: Text)]
        Just req -> do
          -- Get current routes for context
          routes <- liftIO $ getCurrentRoutes state
          let sysPrompt = buildSystemPrompt routes

          -- Add user message to history
          let userMsg = ChatMessage "user" (grMessage req)
          history <- liftIO $ atomically $ do
            h <- readTVar (appHistory state)
            let h' = h ++ [userMsg]
            writeTVar (appHistory state) h'
            return h'

          -- Call Claude
          result <- liftIO $ callClaude sysPrompt history
          case result of
            Left err -> json $ object ["error" .= err]
            Right response -> do
              -- Add assistant response to history
              let assistantMsg = ChatMessage "assistant" response
              liftIO $ atomically $ modifyTVar (appHistory state) (++ [assistantMsg])

              let explanation = extractExplanation response
                  code = extractCodeBlock response
              json $ object
                [ "explanation" .= explanation
                , "code"        .= code
                , "rawResponse" .= response
                ]

    -- Confirm and eval code
    post "/api/confirm" $ do
      body' <- body
      case decode body' :: Maybe ConfirmRequest of
        Nothing -> do
          Web.Scotty.status status400
          json $ object ["error" .= ("Invalid request body" :: Text)]
        Just req -> do
          let code = crCode req
              -- Wrap in progn so swank:eval-and-grab-output evaluates ALL
              -- top-level forms, not just the first one. Without this,
              -- code like "(in-package :living-server) (setf ...)" would
              -- only evaluate the in-package form.
              wrappedCode = wrapInProgn code

          -- Safety check
          case safetyCheck code of
            Just reason -> do
              Web.Scotty.status status403
              json $ object
                [ "success" .= False
                , "error"   .= ("Blocked: " <> reason)
                ]
            Nothing -> do
              mConn <- liftIO $ ensureSwank state
              case mConn of
                Nothing -> json $ object
                  [ "success" .= False
                  , "error"   .= ("Not connected to Lisp server" :: Text)
                  ]
                Just conn -> do
                  result <- liftIO $ swankEval conn wrappedCode
                  case result of
                    Left err -> json $ object
                      [ "success" .= False
                      , "error"   .= err
                      ]
                    Right output -> do
                      -- Persist the original code (not progn-wrapped),
                      -- since `load` handles multiple top-level forms natively.
                      let desc = T.take 50 code
                      _ <- liftIO $ persistCode (appBaseDir state) code desc
                      json $ object
                        [ "success" .= True
                        , "output"  .= output
                        ]

    -- Get chat history (for restoring on page refresh)
    get "/api/history" $ do
      history <- liftIO $ readTVarIO (appHistory state)
      json $ object ["messages" .= history]

    -- Clear chat history
    post "/api/clear-history" $ do
      liftIO $ atomically $ writeTVar (appHistory state) []
      json $ object ["success" .= True]

-- | Try to connect to Swank with retries.
connectSwankRetry :: AppState -> Int -> IO ()
connectSwankRetry _ 0 = return ()
connectSwankRetry state retries = do
  already <- readTVarIO (appSwank state)
  case already of
    Just _ -> return ()  -- already connected
    Nothing -> do
      result <- connectSwank "127.0.0.1" 4005
      case result of
        Right conn -> atomically $ writeTVar (appSwank state) (Just conn)
        Left _ -> do
          threadDelay 2000000  -- 2 seconds
          connectSwankRetry state (retries - 1)

-- | Ensure we have a Swank connection, attempting to connect if needed.
ensureSwank :: AppState -> IO (Maybe SwankConnection)
ensureSwank state = do
  mConn <- readTVarIO (appSwank state)
  case mConn of
    Just conn -> do
      ok <- isConnected conn
      if ok then return (Just conn)
      else do
        atomically $ writeTVar (appSwank state) Nothing
        tryConnect state
    Nothing -> tryConnect state

tryConnect :: AppState -> IO (Maybe SwankConnection)
tryConnect state = do
  running <- isLispRunning (appProcess state)
  if running
    then do
      r <- connectSwank "127.0.0.1" 4005
      case r of
        Right conn -> do
          atomically $ writeTVar (appSwank state) (Just conn)
          return (Just conn)
        Left _ -> return Nothing
    else return Nothing

-- | Get current routes from the Lisp server.
getCurrentRoutes :: AppState -> IO [Route]
getCurrentRoutes state = do
  mConn <- ensureSwank state
  case mConn of
    Nothing -> return []
    Just conn -> do
      result <- swankEval conn "(living-server:list-routes)"
      case result of
        Left _  -> return []
        Right output -> return $ parseLispRoutes output

-- | Parse the Lisp route list format into Route objects.
-- Input format from swank: ((("GET") "/health") (("POST" "GET") "/test"))
-- Each entry is (methods-list path-string).
parseLispRoutes :: Text -> [Route]
parseLispRoutes input =
  let cleaned = T.strip input
  in if T.null cleaned || cleaned == "NIL"
     then []
     else
       -- Extract all quoted strings from the whole expression
       -- They alternate: method strings for each route, then the path
       -- But we need structural parsing to group them correctly.
       parseRouteEntries cleaned

parseRouteEntries :: Text -> [Route]
parseRouteEntries input =
  -- Input: ((("GET") "/health") (("POST") "/test"))
  -- Strip outermost parens to get: (("GET") "/health") (("POST") "/test")
  let inner = stripOuterParens $ T.strip input
      entries = splitAtTopLevelParens (T.unpack inner)
  in map parseOneEntry entries

parseOneEntry :: Text -> Route
parseOneEntry entry =
  -- Entry: (("GET") "/health") or (("GET" "POST") "/users/:id")
  -- Strip outer parens: ("GET") "/health"
  let inner = stripOuterParens $ T.strip entry
      -- Find the methods sublist and the path
      -- The methods are in the first (...), the path is the last quoted string
      allStrings = extractQuotedStrings inner
  in case allStrings of
       [] -> Route ["GET"] "/"
       [single] -> Route ["GET"] single  -- just a path
       xs ->
         -- Last string is the path, everything before is methods
         let path = last xs
             methods = init xs
         in Route (if null methods then ["GET"] else methods) path

extractQuotedStrings :: Text -> [Text]
extractQuotedStrings = go . T.unpack
  where
    go [] = []
    go ('"':rest) =
      let (s, remaining) = readStr rest
      in T.pack s : go remaining
    go (_:rest) = go rest

    readStr [] = ([], [])
    readStr ('\\':c:rest) =
      let (s, r) = readStr rest in (c:s, r)
    readStr ('"':rest) = ([], rest)
    readStr (c:rest) =
      let (s, r) = readStr rest in (c:s, r)

stripOuterParens :: Text -> Text
stripOuterParens t
  | T.isPrefixOf "(" t && T.isSuffixOf ")" t = T.init (T.tail t)
  | otherwise = t

splitAtTopLevelParens :: String -> [Text]
splitAtTopLevelParens [] = []
splitAtTopLevelParens s =
  let s' = dropWhile (\c -> c == ' ' || c == '\n') s
  in case s' of
       ('(':rest) ->
         let (entry, remaining) = collectBalanced 1 rest ""
         in T.pack ("(" ++ entry ++ ")") : splitAtTopLevelParens remaining
       _ -> []

collectBalanced :: Int -> String -> String -> (String, String)
collectBalanced 0 rest acc = (reverse acc, rest)
collectBalanced _ [] acc = (reverse acc, [])
collectBalanced n ('(':rest) acc = collectBalanced (n+1) rest ('(':acc)
collectBalanced n (')':rest) acc
  | n == 1    = (reverse acc, rest)
  | otherwise = collectBalanced (n-1) rest (')':acc)
collectBalanced n ('"':rest) acc =
  -- Handle quoted strings inside parens (preserve them including quotes)
  let (str, remaining) = readQuotedStr rest
      -- str is the content between quotes (in correct order)
      -- acc is in reverse, so we need to add: closing-quote, reversed-str, opening-quote
      acc' = '"' : (reverse str ++ ('"' : acc))
  in collectBalanced n remaining acc'
collectBalanced n (c:rest) acc = collectBalanced n rest (c:acc)

readQuotedStr :: String -> (String, String)
readQuotedStr [] = ([], [])
readQuotedStr ('\\':c:rest) =
  let (s, r) = readQuotedStr rest in ('\\' : c : s, r)
readQuotedStr ('"':rest) = ([], rest)
readQuotedStr (c:rest) =
  let (s, r) = readQuotedStr rest in (c:s, r)

-- | Wrap Lisp code for safe eval via Swank using eval-multi.
-- eval-multi reads and evaluates one form at a time from a string.
-- This means (ql:quickload "dexador") actually runs before the reader
-- tries to read (dex:get ...), avoiding "Package DEX does not exist" errors.
-- The code string is escaped and passed as a Lisp string argument.
wrapInProgn :: Text -> Text
wrapInProgn code =
  "(living-server:eval-multi " <> quoteLispString code <> ")"

-- | Escape a text value as a Lisp string literal.
quoteLispString :: Text -> Text
quoteLispString t =
  let escaped = T.replace "\\" "\\\\" t
      escaped2 = T.replace "\"" "\\\"" escaped
  in "\"" <> escaped2 <> "\""
