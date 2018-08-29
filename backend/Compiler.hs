{-# LANGUAGE OverloadedStrings #-}

module Compiler (init, compile, Result(..)) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock.POSIX as Time
import Prelude hiding (init)
import System.Directory
  ( createDirectoryIfMissing, withCurrentDirectory
  , copyFile, removeFile
  , doesFileExist
  )
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), (<.>))
import System.Process (rawSystem, readProcessWithExitCode)


data Result
    = Success Text Text
    | Error Text
    | Crash Text


init :: IO ()
init = do
    createDirectoryIfMissing True tempDirectory
    copyFile "elm.json" (tempDirectory </> "elm.json")
    code <- rawSystem "elm" ["make"]
    return ()

tempDirectory :: FilePath
tempDirectory = "tmp"

compile :: Text -> IO Result
compile elmSource =
    withCurrentDirectory tempDirectory $ do
        name <- getTempName

        let elmFile = name <.> "elm"
        let jsFile = name <.> "js"

        T.writeFile elmFile (addHeader name elmSource)

        let args = ["make", "--output=" ++ jsFile, elmFile]
        result <- try $ readProcessWithExitCode "elm" args ""

        removeFile elmFile

        case result of
            Left exception ->
                return $ Crash (T.pack $ show (exception :: SomeException))

            Right (ExitFailure _, out, err) ->
                return $ Error (T.pack (out ++ err))

            Right (ExitSuccess, _, _) -> do
                js <- T.readFile jsFile
                removeFile (pathToArtifact name "elmi")
                removeFile (pathToArtifact name "elmo")
                removeFile jsFile
                return $ Success (T.pack name) js

addHeader :: String -> Text -> Text
addHeader name elmSource =
    T.concat [ "module ", T.pack name, " exposing (..)\n", elmSource]

pathToArtifact :: String -> String -> FilePath
pathToArtifact name ext =
    "elm-stuff" </> "build-artifacts" </> "0.19.0"
    </> "user" </> "project" </> "1.0.0"
    </> name <.> ext

getTempName :: IO String
getTempName =
    iterateOnName =<< Time.getPOSIXTime
  where
    iterateOnName time = do
        let name = timeToName time
        exists <- doesFileExist (name <.> "elm")
        if exists
            then
                iterateOnName (time - 60)
            else
                return name

    timeToName time =
        "Temp" ++ show (round (time * 1000000) :: Int)
