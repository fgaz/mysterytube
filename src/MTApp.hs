{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MTApp where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.HTML.Lucid
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import System.Log.FastLogger (ToLogStr, toLogStr, TimedFastLogger)
import Data.Monoid

import Data.IORef

import Lucid (Html)
import Index


mtApp :: FilePath -> FilePath -> TimedFastLogger -> IORef Video -> Application
mtApp staticDir tmpDir logger ref = serve mtAPP $ mtAPPServer staticDir tmpDir logger ref


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

instance ToLogStr Video where
  toLogStr (Video vid vtime) = toLogStr vid
                            <> maybe mempty (("@" <>) . toLogStr . show) vtime

logVideo :: TimedFastLogger -> Video -> IO ()
logVideo logger video = logger $ \time -> toLogStr time
                                       <> " VID "
                                       <> toLogStr video
                                       <> "\n"

type MTAPI = "watch" :> ReqBody '[JSON] Video :> Post '[JSON] Video

type MTAPP = MTAPI
        :<|> "static" :> Raw
        :<|> "gen" :> Raw
        :<|> Get '[HTML] (Html ())

mtAPIServer :: TimedFastLogger -> IORef Video -> Server MTAPI
mtAPIServer log ref = getVideo log ref

mtAPPServer :: FilePath -> FilePath -> TimedFastLogger -> IORef Video -> Server MTAPP
mtAPPServer staticDir tmpDir logger ref =
       mtAPIServer logger ref
  :<|> serveDirectoryWebApp staticDir
  :<|> serveDirectoryWebApp tmpDir
  :<|> return indexPage

charAllowedInVideoId :: Char -> Bool
charAllowedInVideoId char = elem char ("-_" :: String) || isDigit char || isAsciiUpper char || isAsciiLower char

checkVideo :: Video -> Bool
checkVideo (Video vidid time) = idOK && timeOK
  where idOK = Prelude.length vidid == 11 && Prelude.all charAllowedInVideoId vidid
        timeOK = case time of Nothing -> True
                              Just time' -> time' >= 0

getVideo :: TimedFastLogger -> IORef Video -> Video -> Handler Video
getVideo logger ref newVid = if checkVideo newVid
  then liftIO $ do
    logVideo logger newVid
    atomicModifyIORef' ref (\oldVid -> (newVid, oldVid))
  else throwError err400

