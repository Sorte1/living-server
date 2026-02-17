module Main where

import System.Directory (getCurrentDirectory)
import Dashboard.Process (getLispProcess, startLispProcess)
import Dashboard.Server (startDashboard)

main :: IO ()
main = do
  putStrLn "=== Living Server Dashboard ==="
  putStrLn ""

  baseDir <- getCurrentDirectory
  putStrLn $ "Base directory: " ++ baseDir

  -- Create the Lisp process manager
  lispProc <- getLispProcess

  -- Start the SBCL process
  putStrLn "Starting SBCL process..."
  result <- startLispProcess lispProc
  case result of
    Left err -> putStrLn $ "Warning: Could not start SBCL: " ++ show err
    Right () -> putStrLn "SBCL process started."

  -- Start the dashboard web server
  putStrLn ""
  putStrLn "Dashboard running at http://localhost:8080"
  putStrLn "User server running at http://localhost:3001"
  putStrLn ""
  startDashboard lispProc baseDir
