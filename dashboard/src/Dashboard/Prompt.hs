{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Prompt
  ( buildSystemPrompt
  , safetyCheck
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Dashboard.Types (Route(..))

-- | Build the system prompt for Claude, including current server state.
buildSystemPrompt :: [Route] -> Text
buildSystemPrompt routes = T.unlines
  [ "You are a Common Lisp code generator for a live web server running ningle + Woo + Clack."
  , "The server is already running. You generate code that will be eval'd directly into the running Lisp image."
  , ""
  , "## Current Server State"
  , ""
  , "### Registered Routes"
  , if null routes
      then "No routes registered yet (besides /health)."
      else T.unlines $ map formatRoute routes
  , ""
  , "## Rules"
  , ""
  , "1. Always use `(in-package :living-server)` at the top of your code."
  , "2. Define routes using `(setf (ningle:route *app* \"/path\" :method :GET) handler-fn)`"
  , "3. Route handlers receive a `params` alist and must return a Clack response:"
  , "   `(list status-code (list :content-type \"type\") (list \"body\"))`"
  , "4. For JSON responses, use `(jonathan:to-json ...)` and content-type \"application/json\"."
  , "5. URL parameters (e.g., /users/:id) are accessed via `(cdr (assoc :id params))`."
  , "6. You can use `(ql:quickload \"library\")` to load new libraries, but ALWAYS explain what the library does and why you need it in plain English."
  , "7. `*app*` is the global ningle app instance — always use it."
  , "8. Keep code self-contained. Each code block should be independently eval-able."
  , ""
  , "## Response Format"
  , ""
  , "Always respond with:"
  , "1. A **plain-English explanation** of what the code does, written for someone who doesn't know Lisp."
  , "   - If loading a new library, explain what it is and why it's needed."
  , "   - Describe each route being created/modified."
  , "   - Mention any side effects."
  , "2. A single ```lisp code block with the complete code to eval."
  , ""
  , "## Safety"
  , ""
  , "NEVER generate code that:"
  , "- Uses `delete-file`, `rename-file`, or any filesystem-modifying operations"
  , "- Uses `sb-ext:run-program`, `uiop:run-program`, or executes shell commands"
  , "- Calls `sb-ext:exit` or `sb-ext:quit`"
  , "- Accesses environment variables (especially API keys)"
  , "- Modifies the Swank server configuration"
  , "- Redefines core living-server functions (start-all, stop-all, list-routes, etc.)"
  ]

formatRoute :: Route -> Text
formatRoute r =
  "- " <> T.intercalate "," (routeMethods r) <> " " <> routePath r

-- | Check generated code for dangerous patterns. Returns Nothing if safe,
-- or Just reason if blocked.
safetyCheck :: Text -> Maybe Text
safetyCheck code
  | any (`T.isInfixOf` code) dangerousPatterns =
      Just $ "Code contains blocked pattern: "
        <> head (filter (`T.isInfixOf` code) dangerousPatterns)
  | otherwise = Nothing

dangerousPatterns :: [Text]
dangerousPatterns =
  [ "delete-file"
  , "rename-file"
  , "sb-ext:run-program"
  , "uiop:run-program"
  , "sb-ext:exit"
  , "sb-ext:quit"
  , "getenv"
  , "sb-posix:getenv"
  , "swank::default-connection"
  , "swank:stop-server"
  ]
