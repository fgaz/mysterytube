{-# LANGUAGE OverloadedStrings #-}
module Index where

import Lucid
import Data.Text (Text)

indexPage :: Html ()
indexPage = do
  doctypehtml_ ""
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ "MysteryTube"
    link_ [href_ "/static/normalize.css", rel_ "stylesheet", type_ "text/css"]
    link_ [href_ "/static/style.css", rel_ "stylesheet", type_ "text/css"]
    script_ [src_ "/gen/api.js"] ("" :: Text)
    script_ [src_ "/static/main.js"] ("" :: Text)
  body_ $ do
    main_ $ do
      h1_ "MysteryTube"
      div_ [id_ "errors"] ""
      div_ [id_ "form"] $ do
        input_ [ id_ "vidtxt"
               , type_ "text"
               , title_ "Link to youtube video"
               , placeholder_ "Link to youtube video"
               , pattern_ "^(?:https?:\\/\\/)?(?:m\\.|www\\.)?(?:youtu\\.be\\/|youtube\\.com\\/(?:embed\\/|v\\/|watch\\?v=|watch\\?.+&v=))((\\w|-){11})(?:\\S+)?$"
               , onkeyup_ "if(event.keyCode==13) requestVideo();" ]
        button_ [id_ "submit", onclick_ "requestVideo();"] $ "Watch"
      div_ [id_ "vid"] $ img_ [id_ "fakevid", src_ "/static/fakevid.png", width_ "560", height_ "315"]
    footer_ $ do
      p_ $ do
        "Developed by "
        a_ [href_ "http://fgaz.me"] "fgaz"
        " - "
        a_ [href_ "https://github.com/fgaz/mysterytube"] "Sources"


