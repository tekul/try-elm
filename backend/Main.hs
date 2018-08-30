{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Catch (catchIf)
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics hiding (moduleName)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import System.IO.Error (isDoesNotExistError)
import Text.Blaze.Html5 (Html, preEscapedText)
import Web.Internal.FormUrlEncoded (FromForm)

import qualified Compiler
import qualified Generate

newtype ElmSource = ElmSource { code :: Text } deriving (Generic)

deriving instance MimeUnrender PlainText ElmSource
instance FromForm ElmSource

type Api =
         "compile" :> ReqBody '[PlainText, FormUrlEncoded] ElmSource :> Post '[HTML] Html
    :<|> "examples" :> Capture "exampleName" Text :> Get '[HTML] Html
    :<|> Raw

api :: Proxy Api
api = Proxy

server :: Server Api
server =
         compile
    :<|> examples
    :<|> serveDirectoryWebApp "static"

compile :: ElmSource -> Handler Html
compile (ElmSource src)= do
    result <- liftIO $ Compiler.compile src
    return $ case result of
        Compiler.Success moduleName js -> Generate.compilerSuccess moduleName js
        Compiler.Error msg -> Generate.compilerError msg
        Compiler.Crash msg -> Generate.compilerError msg

examples :: Text -> Handler Html
examples name =
    serveFile $ "static/examples/" ++ T.unpack name ++ ".html"

serveFile :: FilePath -> Handler Html
serveFile name = do
    liftIO $ print name
    catchIf isDoesNotExistError (liftIO $ preEscapedText <$> T.readFile name) (const notFound)

notFound :: Handler Html
notFound = throwError err404

app :: Application
app = serve api server

main :: IO ()
main = Compiler.init >> run 8000 app
