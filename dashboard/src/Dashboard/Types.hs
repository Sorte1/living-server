{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data ServerStatus = ServerStatus
  { ssRunning   :: Bool
  , ssHttpPort  :: Int
  , ssSwankPort :: Int
  } deriving (Show, Generic)

instance ToJSON ServerStatus where
  toJSON s = object
    [ "running"   .= ssRunning s
    , "httpPort"   .= ssHttpPort s
    , "swankPort"  .= ssSwankPort s
    ]

data Route = Route
  { routeMethods :: [Text]
  , routePath    :: Text
  } deriving (Show, Generic)

instance ToJSON Route where
  toJSON r = object
    [ "methods" .= routeMethods r
    , "path"    .= routePath r
    ]

data EvalResult = EvalResult
  { evalSuccess :: Bool
  , evalOutput  :: Text
  , evalError   :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON EvalResult where
  toJSON e = object
    [ "success" .= evalSuccess e
    , "output"  .= evalOutput e
    , "error"   .= evalError e
    ]

data GenerateRequest = GenerateRequest
  { grMessage :: Text
  } deriving (Show, Generic)

instance FromJSON GenerateRequest where
  parseJSON = withObject "GenerateRequest" $ \v ->
    GenerateRequest <$> v .: "message"

data GenerateResponse = GenerateResponse
  { genExplanation :: Text
  , genCode        :: Text
  , genRawResponse :: Text
  } deriving (Show, Generic)

instance ToJSON GenerateResponse where
  toJSON g = object
    [ "explanation" .= genExplanation g
    , "code"        .= genCode g
    , "rawResponse" .= genRawResponse g
    ]

data ConfirmRequest = ConfirmRequest
  { crCode :: Text
  } deriving (Show, Generic)

instance FromJSON ConfirmRequest where
  parseJSON = withObject "ConfirmRequest" $ \v ->
    ConfirmRequest <$> v .: "code"

data ChatMessage = ChatMessage
  { cmRole    :: Text
  , cmContent :: Text
  } deriving (Show, Generic)

instance ToJSON ChatMessage where
  toJSON m = object
    [ "role"    .= cmRole m
    , "content" .= cmContent m
    ]

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v ->
    ChatMessage <$> v .: "role" <*> v .: "content"
