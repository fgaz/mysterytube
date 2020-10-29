module Main where

import Network.Wai.Handler.Warp
import Servant.JS
import Data.IORef (newIORef)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import System.Log.FastLogger (TimedFastLogger, withTimedFastLogger, LogType'(LogStdout), defaultBufSize)
import System.Log.FastLogger.Date (newTimeCache, simpleTimeFormat)

import Paths_mysterytube
import MTApp

portEnv :: String
portEnv = "MYSTERYTUBE_PORT"
hostEnv :: String
hostEnv = "MYSTERYTUBE_IP"
defaultPort :: Int
defaultPort = 8080
defaultHost :: String
defaultHost = "127.0.0.1"
defaultVideoId :: String
defaultVideoId = "dQw4w9WgXcQ"
staticDirEnv :: String
staticDirEnv = "MYSTERYTUBE_STATIC_DIR"
tmpDirEnv :: String
tmpDirEnv = "MYSTERYTUBE_TMP_DIR"

main :: IO ()
main = setup $ \tmpDir logger -> do
  envPort <- lookupEnv portEnv
  envHost <- lookupEnv hostEnv
  let port = fromMaybe defaultPort $ read <$> envPort
  let host = fromString $ fromMaybe defaultHost envHost

  envStaticDir <- lookupEnv staticDirEnv
  staticDir <- (</>"static") <$> getDataDir
  let staticDir' = fromMaybe staticDir envStaticDir
  envTmpDir <- lookupEnv tmpDirEnv
  let tmpDir' = fromMaybe tmpDir envTmpDir

  writeJSFiles tmpDir'
  ref <- newIORef Video { videoId = defaultVideoId, videoTime = Nothing }
  runSettings (setPort port $ setHost host defaultSettings) $ mtApp staticDir' tmpDir' logger ref

setup :: (FilePath -> TimedFastLogger -> IO ()) -> IO ()
setup f = do
  timeCache <- newTimeCache simpleTimeFormat
  withSystemTempDirectory "mysterytube-" $
    \tmpDir -> withTimedFastLogger timeCache (LogStdout defaultBufSize) $
      \logger -> f tmpDir logger

writeJSFiles :: FilePath -> IO ()
writeJSFiles staticDir = --do
  writeJSForAPI mtAPI vanillaJS $ staticDir </> "./api.js"

