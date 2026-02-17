{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dashboard.Claude
  ( callClaude
  , extractCodeBlock
  , extractExplanation
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
import System.Environment (lookupEnv)

import Dashboard.Types (ChatMessage(..))

-- | Call the Anthropic Messages API with conversation history.
callClaude :: Text -> [ChatMessage] -> IO (Either Text Text)
callClaude systemPrompt messages = do
  mApiKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mApiKey of
    Nothing -> return $ Left "ANTHROPIC_API_KEY environment variable not set"
    Just apiKey -> do
      manager <- newManager tlsManagerSettings
      let body = object
            [ "model"      .= ("claude-sonnet-4-5-20250929" :: Text)
            , "max_tokens" .= (4096 :: Int)
            , "system"     .= systemPrompt
            , "messages"   .= map chatMessageToJSON messages
            ]
      result <- try $ do
        initReq <- parseRequest "https://api.anthropic.com/v1/messages"
        let req = initReq
              { method = "POST"
              , requestHeaders =
                  [ (hContentType, "application/json")
                  , ("x-api-key", TE.encodeUtf8 (T.pack apiKey))
                  , ("anthropic-version", "2023-06-01")
                  ]
              , requestBody = RequestBodyLBS (encode body)
              }
        resp <- httpLbs req manager
        return $ responseBody resp
      case result of
        Left (e :: SomeException) ->
          return $ Left $ "API request failed: " <> T.pack (show e)
        Right respBody ->
          case decode respBody :: Maybe Value of
            Nothing -> return $ Left "Failed to parse API response"
            Just (Object obj) ->
              case KM.lookup "content" obj of
                Just (Array arr) ->
                  case arr of
                    v | not (null v) ->
                        case head (toList v) of
                          Object block ->
                            case KM.lookup "text" block of
                              Just (String txt) -> return $ Right txt
                              _ -> return $ Left "No text in response content block"
                          _ -> return $ Left "Unexpected content block format"
                      | otherwise -> return $ Left "Empty content array"
                Just _ -> return $ Left "Content is not an array"
                Nothing ->
                  case KM.lookup "error" obj of
                    Just err -> return $ Left $ "API error: " <> T.pack (show err)
                    Nothing  -> return $ Left $ "Unexpected response: " <> T.pack (show obj)
            _ -> return $ Left "Response is not a JSON object"
  where
    toList = foldr (:) []

chatMessageToJSON :: ChatMessage -> Value
chatMessageToJSON (ChatMessage role content) =
  object ["role" .= role, "content" .= content]

-- | Extract a Lisp code block from Claude's markdown response.
-- Looks for ```lisp or ```common-lisp fenced blocks.
extractCodeBlock :: Text -> Maybe Text
extractCodeBlock response =
  let lines' = T.lines response
      -- Find code blocks with lisp language tag
      blocks = findCodeBlocks lines'
  in case blocks of
       (b:_) -> Just b
       []    -> Nothing

findCodeBlocks :: [Text] -> [Text]
findCodeBlocks [] = []
findCodeBlocks (l:ls)
  | isLispFence l =
      let (block, rest) = span (not . isClosingFence) ls
          code = T.unlines block
      in T.strip code : findCodeBlocks (drop 1 rest)
  | otherwise = findCodeBlocks ls

isLispFence :: Text -> Bool
isLispFence l =
  let stripped = T.strip l
  in stripped == "```lisp"
     || stripped == "```common-lisp"
     || stripped == "```cl"

isClosingFence :: Text -> Bool
isClosingFence l = T.strip l == "```"

-- | Extract the explanation text (everything outside code blocks).
extractExplanation :: Text -> Text
extractExplanation response =
  let lines' = T.lines response
      explanationLines = filterOutCodeBlocks lines' False
  in T.strip $ T.unlines explanationLines

filterOutCodeBlocks :: [Text] -> Bool -> [Text]
filterOutCodeBlocks [] _ = []
filterOutCodeBlocks (l:ls) inBlock
  | inBlock =
      if isClosingFence l
        then filterOutCodeBlocks ls False
        else filterOutCodeBlocks ls True
  | isLispFence l = filterOutCodeBlocks ls True
  | otherwise = l : filterOutCodeBlocks ls False
