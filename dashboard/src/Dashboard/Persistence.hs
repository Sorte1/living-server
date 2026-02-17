{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Persistence
  ( persistCode
  , getNextFileIndex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import Data.List (sort)
import Data.Char (isDigit)
import Text.Printf (printf)

-- | Persist generated code to a file in server/generated/routes/ and update the manifest.
persistCode :: FilePath    -- ^ Project base directory
            -> Text        -- ^ The Lisp code to persist
            -> Text        -- ^ A short description for the filename
            -> IO FilePath -- ^ Path to the written file
persistCode baseDir code description = do
  let routesDir = baseDir </> "server" </> "generated" </> "routes"
  createDirectoryIfMissing True routesDir

  idx <- getNextFileIndex routesDir
  let safeName = sanitizeFilename description
      fileName = printf "%03d" idx ++ "-" ++ safeName ++ ".lisp"
      filePath = routesDir </> fileName

  TIO.writeFile filePath code
  updateManifest baseDir fileName
  return filePath

-- | Get the next sequential file index from the routes directory.
getNextFileIndex :: FilePath -> IO Int
getNextFileIndex dir = do
  files <- listDirectory dir
  let lispFiles = filter (\f -> takeExtension f == ".lisp") files
      indices = map extractIndex (sort lispFiles)
      maxIdx = if null indices then 0 else maximum indices
  return (maxIdx + 1)

extractIndex :: FilePath -> Int
extractIndex name =
  let digits = takeWhile isDigit name
  in if null digits then 0 else read digits

sanitizeFilename :: Text -> String
sanitizeFilename t =
  let words' = T.words $ T.toLower t
      -- Take first few words, keep only alphanum and hyphens
      cleaned = map (T.filter (\c -> c `elem` (['a'..'z'] ++ ['0'..'9'] ++ ['-']))) words'
      nonEmpty = filter (not . T.null) cleaned
  in T.unpack $ T.intercalate "-" (take 4 nonEmpty)

-- | Update the manifest.lisp to include the new file.
updateManifest :: FilePath -> FilePath -> IO ()
updateManifest baseDir fileName = do
  let manifestPath = baseDir </> "server" </> "generated" </> "manifest.lisp"
      loadLine = "(load (merge-pathnames \"generated/routes/"
                 ++ fileName
                 ++ "\" (asdf:system-source-directory \"living-server\")))"
  contents <- TIO.readFile manifestPath
  let newContents = contents <> "\n" <> T.pack loadLine
  TIO.writeFile manifestPath newContents
