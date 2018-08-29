{-# LANGUAGE OverloadedStrings #-}
module Generate
  ( Highlight(..)
  , serverHtml, compilerSuccess, compilerError
  )
  where

import qualified Data.Aeson as Json
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Blaze as Blaze
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



data Highlight = Highlight | NoHighlight


-- LOCAL HTML


serverHtml :: Text -> Text -> H.Html
serverHtml name jsSource =
    htmlSkeleton Highlight name $ do
        H.script $ Blaze.preEscapedToMarkup jsSource
        H.div ! A.id "app" $ ""
        H.script "var runningElmModule = Elm.Main.init({node: document.getElementById('app')});"



-- FOREIGN HTML


compilerSuccess :: Text -> Text -> H.Html
compilerSuccess moduleName jsSource =
    htmlSkeleton NoHighlight moduleName $ do
        H.script $ Blaze.preEscapedText jsSource
        H.div ! A.id "app" $ ""
        H.script $ Blaze.preEscapedText $
            T.concat
                [ "var runningElmModule = Elm."
                , moduleName
                , ".init({node: document.getElementById('app')});"
                ]


compilerError :: Text -> H.Html
compilerError errorJson =
    htmlSkeleton Highlight "Oops!" $ do
        H.script ! A.src "/editor/errors.js" $ ""
        H.script $ Blaze.text (initErrorScreen errorJson)


initErrorScreen :: Text -> Text
initErrorScreen errorJson =
    T.concat
        [ "var errors = Elm.Errors.init( {flags: "
        , T.decodeUtf8 (toStrict (Json.encode errorJson))
        , "});"
        ]





-- CREATE HTML DOCUMENTS


htmlSkeleton :: Highlight -> Text -> H.Html -> H.Html
htmlSkeleton highlight title scripts =
    H.docTypeHtml $ do
        H.head $ do
            H.meta ! A.charset "UTF-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            H.title (H.toHtml title)
            favicon
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/style.css?v=4"

            case highlight of
                Highlight -> do
                    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/highlight/styles/default.css"
                    H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""

                NoHighlight ->
                    return ()

        H.body scripts


favicon :: H.Html
favicon =
    H.link
        ! A.rel "shortcut icon"
        ! A.sizes "16x16 32x32 48x48 64x64 128x128 256x256"
        ! A.href "/favicon.ico"
