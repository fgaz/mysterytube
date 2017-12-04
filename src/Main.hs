module Main where

import Network.Wai.Handler.Warp
import Data.IORef (newIORef)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import Data.String (fromString)

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

main :: IO ()
main = do
  ref <- newIORef Video { videoId = defaultVideoId, videoTime = Nothing }
  envPort <- lookupEnv portEnv
  envHost <- lookupEnv hostEnv
  let port = fromMaybe defaultPort $ read <$> envPort
  let host = fromString $ fromMaybe defaultHost envHost
  envHost <- lookupEnv staticDirEnv
  staticDir <- (</>"static") <$> getDataDir
  let staticDir' = fromMaybe staticDir envHost
  writeJSFiles staticDir
  runSettings (setPort port $ setHost host defaultSettings) $ mtApp staticDir' ref

