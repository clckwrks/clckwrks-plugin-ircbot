{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.IrcBot.Page.Template where

import Clckwrks
import Clckwrks.Plugin
import Control.Monad.State (get)
import Clckwrks.IrcBot.Monad
import Control.Monad.Reader
import Data.Text (Text)
import HSP.XML
import HSP.XMLGenerator
import Happstack.Server.HSP.HTML ()
import Web.Plugins.Core          (Plugin(..), getPluginRouteFn, getTheme)

template :: ( EmbedAsChild IrcBotM headers
            , EmbedAsChild IrcBotM body
            ) =>
            Text
         -> headers
         -> body
         -> IrcBotM Response
template ttl hdrs bdy =
    do p <- plugins <$> get
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild hdrs -- <%> <link rel="stylesheet" type="text/css" href=(IrcBotData "style.css") /> <% hdrs %></%>
       bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
       fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ (themeTemplate p (ThemeStyleId 0) ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT IrcBotConfig (ServerPartT IO) (a, ClckState)
      f m = ReaderT $ \_ -> m
