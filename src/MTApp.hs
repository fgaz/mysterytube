{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MTApp where

import Control.Monad.Except
import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Servant
import Servant.JS
import Data.Text as T
import Data.Text.IO as T
import Servant.HTML.Lucid
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import System.FilePath ((</>))

import Data.IORef

import Lucid (Html)
import Index


mtApp :: FilePath -> IORef Video -> Application
mtApp staticPath ref = serve mtAPP $ mtAPPServer staticPath ref


mtAPI :: Proxy MTAPI
mtAPI = Proxy

mtAPP :: Proxy MTAPP
mtAPP = Proxy

type Time = Int

data Video = Video { videoId :: String
                   , videoTime :: Maybe Time
                   } deriving Generic

instance ToJSON Video
instance FromJSON Video

type MTAPI = "watch" :> ReqBody '[JSON] Video :> Post '[JSON] Video

type MTAPP = MTAPI
        :<|> "static" :> Raw
        :<|> Get '[HTML] (Html ())

mtAPIServer :: IORef Video -> Server MTAPI
mtAPIServer ref = getVideo ref

mtAPPServer :: FilePath -> IORef Video -> Server MTAPP
mtAPPServer staticPath ref = mtAPIServer ref
                        :<|> serveDirectoryWebApp staticPath
                        :<|> return indexPage

charAllowedInVideoId :: Char -> Bool
charAllowedInVideoId char = elem char ("-_" :: String) || isDigit char || isAsciiUpper char || isAsciiLower char

checkVideo :: Video -> Bool
checkVideo (Video vidid time) = idOK && timeOK
  where idOK = Prelude.length vidid == 11 && Prelude.all charAllowedInVideoId vidid
        timeOK = case time of Nothing -> True
                              Just time' -> time' >= 0

getVideo :: IORef Video -> Video -> Handler Video
getVideo ref newVid = if checkVideo newVid
  then liftIO $ atomicModifyIORef' ref (\oldVid -> (newVid, oldVid))
  else throwError err400

-- js --

-- FIXME use temp dir or put it in memory
writeJSFiles :: FilePath -> IO ()
writeJSFiles staticDir = --do
  writeJSForAPI mtAPI vanillaJS $ staticDir </> "./api.js"

