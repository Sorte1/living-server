{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dashboard.Process
  ( LispProcess
  , startLispProcess
  , stopLispProcess
  , isLispRunning
  , getLispProcess
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (try, SomeException, catch)
import Data.List (intercalate, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import System.Process

data LispProcess = LispProcess
  { lpHandle  :: TVar (Maybe ProcessHandle)
  , lpBaseDir :: FilePath
  }

-- | Create a LispProcess manager.
newLispProcess :: FilePath -> IO LispProcess
newLispProcess baseDir = do
  handleVar <- newTVarIO Nothing
  return $ LispProcess handleVar baseDir

-- | Get or create the LispProcess manager.
getLispProcess :: IO LispProcess
getLispProcess = do
  dir <- getCurrentDirectory
  newLispProcess dir

-- | Start the SBCL process with the living-server system.
-- Uses boot.lisp to avoid reader errors with package references.
startLispProcess :: LispProcess -> IO (Either Text ())
startLispProcess lp = do
  running <- isLispRunning lp
  if running
    then return $ Left "Lisp process is already running"
    else do
      let bootScript = lpBaseDir lp </> "server" </> "boot.lisp"
      -- Ensure SBCL uses the system C compiler, not any wasm SDK clang
      parentEnv <- getEnvironment
      let childEnv = setEnvVar "CC" "/usr/bin/clang"
                   $ fixPath parentEnv
      result <- try $ createProcess (proc "sbcl"
            [ "--non-interactive"
            , "--load", bootScript
            ])
            { env = Just childEnv
            }
      case result of
        Left (e :: SomeException) ->
          return $ Left $ "Failed to start SBCL: " <> T.pack (show e)
        Right (_, _, _, ph) -> do
          atomically $ writeTVar (lpHandle lp) (Just ph)
          -- Give it time to start up and compile dependencies
          threadDelay 8000000  -- 8 seconds for first load
          return $ Right ()

-- | Stop the SBCL process.
stopLispProcess :: LispProcess -> IO ()
stopLispProcess lp = do
  mHandle <- atomically $ do
    h <- readTVar (lpHandle lp)
    writeTVar (lpHandle lp) Nothing
    return h
  case mHandle of
    Just ph -> do
      terminateProcess ph `catch` (\(_ :: SomeException) -> return ())
      _ <- waitForProcess ph `catch` (\(_ :: SomeException) -> return ExitSuccess)
      return ()
    Nothing -> return ()

-- | Check if the SBCL process is running.
isLispRunning :: LispProcess -> IO Bool
isLispRunning lp = do
  mHandle <- readTVarIO (lpHandle lp)
  case mHandle of
    Nothing -> return False
    Just ph -> do
      mCode <- getProcessExitCode ph
      case mCode of
        Nothing -> return True   -- still running
        Just _  -> do
          atomically $ writeTVar (lpHandle lp) Nothing
          return False

-- | Fix PATH to prefer system compilers over wasm SDK.
fixPath :: [(String, String)] -> [(String, String)]
fixPath = map fixEntry
  where
    fixEntry ("PATH", v) =
      let parts = splitOn ':' v
          filtered = filter (not . isWasmPath) parts
          prefixed = "/usr/bin" : "/usr/local/bin" : "/opt/homebrew/bin" : filtered
      in ("PATH", intercalate ":" prefixed)
    fixEntry kv = kv

    isWasmPath p = "wasi-sdk" `isInfixOfStr` p || ".ghc-wasm" `isInfixOfStr` p

    isInfixOfStr needle haystack = any (isPrefixOf needle) (tails' haystack)
    tails' [] = [[]]
    tails' s@(_:xs) = s : tails' xs

    splitOn _ [] = [""]
    splitOn sep (c:cs)
      | c == sep  = "" : splitOn sep cs
      | otherwise = case splitOn sep cs of
                      (h:t) -> (c:h) : t
                      []    -> [[c]]

-- | Set or replace an environment variable.
setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key val env' =
  (key, val) : filter (\(k, _) -> k /= key) env'
