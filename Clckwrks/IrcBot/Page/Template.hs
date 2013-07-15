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
       mTheme <- getTheme p
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       case mTheme of
         Nothing      -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
         (Just theme) ->
             do hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild hdrs -- <%> <link rel="stylesheet" type="text/css" href=(IrcBotData "style.css") /> <% hdrs %></%>
                bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
                fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ unXMLGenT $ (_themeTemplate theme ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT IrcBotConfig (ServerPartT IO) (a, ClckState)
      f m = ReaderT $ \_ -> m

{-

template :: ( EmbedAsChild IrcBotM headers
            , EmbedAsChild IrcBotM body
            ) =>
            String
         -> headers
         -> body
         -> IrcBotM Response
template ttl hdrs bdy =
    do pageTemplate <- ircBotPageTemplate <$> ask
       fmap toResponse $ unXMLGenT $
            pageTemplate ttl <%> <link rel="stylesheet" type="text/css" href=(DarcsData "style.css") /> <% hdrs %></%> bdy
-}
{-

template :: ( EmbedAsChild (Clck ClckURL) headers
            , EmbedAsChild (Clck ClckURL) body
            ) =>
            String
         -> headers
         -> body
         -> IrcBotM Response
template ttl hdrs bdy =
    do pageTemplate <- ircBotPageTemplate <$> ask
       fmap toResponse $ mapClckT lift $ unXMLGenT $
            pageTemplate ttl hdrs bdy

-}