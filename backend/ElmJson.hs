{-# LANGUAGE OverloadedStrings #-}

module ElmJson (defaultBuild) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)


defaultBuild :: Text
defaultBuild = decodeUtf8 . toStrict $ encode defaultBuildJson

defaultBuildJson :: Value
defaultBuildJson = object
    [ ("type", "application")
    , "source-directories" .= [ "." :: Text ]
    , ("elm-version", "0.19.0")
    , "dependencies" .= object
        [ "direct" .= object
            [ ("elm/browser", "1.0.0")
            , ("elm/core", "1.0.0")
            , ("elm/html", "1.0.0")
            , ("elm/http", "1.0.0")
            , ("elm/json", "1.0.0")
            , ("elm/random", "1.0.0")
            , ("elm/svg", "1.0.0")
            , ("elm/time", "1.0.0")
            , ("elm-explorations/markdown", "1.0.0")
            ]
        , "indirect" .= object
            [ ("elm/url", "1.0.0")
            , ("elm/virtual-dom", "1.0.0")
            ]
        ]

    , "test-dependencies" .= object
        [ "direct" .= object []
        , "indirect" .= object []
        ]
    ]
