{-# LANGUAGE OverloadedStrings #-}

module Dashboard.SwankProtocol
  ( encodeMessage
  , decodeMessage
  , makeEvalForm
  , makeConnectionInfoForm
  , parseEvalResult
  , extractQuotedStrings
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex, readHex)

-- | Encode a Swank message with 6-char hex length prefix.
-- The Swank wire format is: HHHHHHPAYLOAD
-- where HHHHHH is the length of PAYLOAD in hex, zero-padded to 6 chars.
encodeMessage :: Text -> BS.ByteString
encodeMessage msg =
  let payload = TE.encodeUtf8 msg
      len = BS.length payload
      hexLen = leftPad 6 '0' (showHex len "")
  in BS8.pack hexLen <> payload

-- | Decode a Swank message: read 6-char hex length, then read that many bytes.
-- Returns (decoded message, remaining bytes).
decodeMessage :: BS.ByteString -> Maybe (Text, BS.ByteString)
decodeMessage bs
  | BS.length bs < 6 = Nothing
  | otherwise =
      let (hexPart, rest) = BS.splitAt 6 bs
          hexStr = BS8.unpack hexPart
      in case readHex hexStr of
           [(len, "")] ->
             if BS.length rest >= len
               then let (payload, remaining) = BS.splitAt len rest
                    in Just (TE.decodeLatin1 payload, remaining)
               else Nothing
           _ -> Nothing

-- | Build the S-expression for a Swank connection-info request.
makeConnectionInfoForm :: Int -> Text
makeConnectionInfoForm counter =
  "(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t "
  <> T.pack (show counter)
  <> ")"

-- | Build the S-expression for a Swank eval request.
-- Uses (:emacs-rex (swank:eval-and-grab-output ...) "LIVING-SERVER" t counter)
-- Evaluating in the LIVING-SERVER package means symbols like *app* resolve correctly.
makeEvalForm :: Text -> Int -> Text
makeEvalForm code counter =
  "(:emacs-rex (swank:eval-and-grab-output "
  <> quoteLisp code
  <> ") \"LIVING-SERVER\" t "
  <> T.pack (show counter)
  <> ")"

-- | Quote a string for Lisp: escape backslashes and double quotes, wrap in double quotes.
quoteLisp :: Text -> Text
quoteLisp t =
  let escaped = T.replace "\\" "\\\\" t
      escaped2 = T.replace "\"" "\\\"" escaped
  in "\"" <> escaped2 <> "\""

-- | Parse the result from a Swank :return message.
-- Expected format: (:return (:ok ("output" "value")) counter)
-- Returns (output, value) on success, or an error message.
parseEvalResult :: Text -> Either Text (Text, Text)
parseEvalResult msg =
  let trimmed = T.strip msg
  in if ":return" `T.isInfixOf` trimmed
     then if ":ok" `T.isInfixOf` trimmed
          then let values = extractQuotedStrings trimmed
               in case values of
                    (v1:v2:_) -> Right (v1, v2)
                    [v1]      -> Right ("", v1)
                    []        -> Right ("", "")
          else if ":abort" `T.isInfixOf` trimmed
               then Left $ "Eval aborted: " <> trimmed
               else Left $ "Eval failed: " <> trimmed
     else Left $ "Unexpected response: " <> trimmed

-- | Extract all double-quoted strings from an S-expression.
extractQuotedStrings :: Text -> [Text]
extractQuotedStrings = go . T.unpack
  where
    go [] = []
    go ('"':rest) =
      let (str, remaining) = readQuoted rest
      in T.pack str : go remaining
    go (_:rest) = go rest

    readQuoted [] = ([], [])
    readQuoted ('\\':c:rest) =
      let (str, remaining) = readQuoted rest
      in (c:str, remaining)
    readQuoted ('"':rest) = ([], rest)
    readQuoted (c:rest) =
      let (str, remaining) = readQuoted rest
      in (c:str, remaining)

leftPad :: Int -> Char -> String -> String
leftPad n c s = replicate (max 0 (n - length s)) c ++ s
