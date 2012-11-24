{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.IrcBot.Plugin where

import Clckwrks
import Clckwrks.Plugin               (clckPlugin)
import Clckwrks.IrcBot.URL           (IrcBotURL(..))
import Clckwrks.IrcBot.Acid          (GetIrcConfig(..), initialIrcBotState)
import Clckwrks.IrcBot.Monad         (IrcBotConfig(..), runIrcBotT)
import Clckwrks.IrcBot.Route         (routeIrcBot)
import Clckwrks.IrcBot.Types         (IrcConfig(..), emptyIrcConfig)
import Control.Concurrent            (ThreadId, killThread)
import Data.Acid                     as Acid
import Data.Acid.Local               (createCheckpointAndClose, openLocalStateFrom)
import Data.Text                     (Text)
import qualified Data.Text.Lazy      as TL
import Data.Maybe                    (fromMaybe)
import Data.Set                      (Set)
import Network                       (PortID(PortNumber))
import Network.IRC.Bot.BotMonad      (BotMonad(..))
import Network.IRC.Bot.Core          as IRC (BotConf(..), User(..), nullBotConf, simpleBot)
import Network.IRC.Bot.Log           (LogLevel(..), nullLogger, stdoutLogger)
import Network.IRC.Bot.Part.Dice     (dicePart)
import Network.IRC.Bot.Part.Hello    (helloPart)
import Network.IRC.Bot.Part.Ping     (pingPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import Network.IRC.Bot.PosixLogger   (posixLogger)
import System.FilePath               ((</>))
import Web.Plugin.Core               (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, initPlugin, getConfig, getPluginRouteFn)
import Paths_clckwrks_plugin_ircbot  (getDataDir)

ircBotHandler :: (IrcBotURL -> [(Text, Maybe Text)] -> Text)
              -> IrcBotConfig
              -> ClckPlugins
              -> [Text]
              -> ClckT ClckURL (ServerPartT IO) Response
ircBotHandler showIrcBotURL ircBotConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runIrcBotT ircBotConfig $ routeIrcBot u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (IrcBotURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showIrcBotURL u p

ircBotInit :: ClckPlugins
           -> IO (Maybe Text)
ircBotInit plugins =
    do (Just ircBotShowFn) <- getPluginRouteFn plugins (pluginName ircBotPlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath  = maybe "_state"   (\td -> td </> "_state")   mTopDir -- FIXME
           ircLogDir = maybe "_irclogs" (\td -> td </> "_irclogs") mTopDir
       acid <- openLocalStateFrom (basePath </> "ircBot") (initialIrcBotState emptyIrcConfig)
       addCleanup plugins Always (createCheckpointAndClose acid)
       reconnect <- botConnect plugins acid ircLogDir
       let ircBotConfig = IrcBotConfig { ircBotLogDirectory = ircLogDir
                                       , ircBotState        = acid
                                       , ircBotClckURL      = clckShowFn
                                       , ircReconnect       = reconnect
                                       }

--       addPreProc plugins (ircBotCmd ircBotShowFn)
       addHandler plugins (pluginName ircBotPlugin) (ircBotHandler ircBotShowFn ircBotConfig)
       return Nothing

botConnect :: Plugins theme n hook config st
           -> Acid.AcidState (Acid.EventState GetIrcConfig)
           -> FilePath
           -> IO (IO ())
botConnect plugins ircBot ircBotLogDir =
    do ic@IrcConfig{..} <- Acid.query ircBot GetIrcConfig
       let botConf = nullBotConf { channelLogger = Just $ posixLogger (Just ircBotLogDir) "#happs"
                                 , IRC.host      = ircHost
                                 , IRC.port      = PortNumber $ fromIntegral ircPort
                                 , nick          = ircNick
                                 , commandPrefix = ircCommandPrefix
                                 , user          = ircUser
                                 , channels      = ircChannels
                                 , limits        = Just (5, 2000000)
                                 }
       ircParts <- initParts (channels botConf)
       (tids, reconnect) <- simpleBot botConf ircParts
       addCleanup plugins Always (mapM_ killThread tids)
       return reconnect

initParts :: (BotMonad m) =>
             Set String  -- ^ set of channels to join
          -> IO [m ()]
initParts chans =
    do (_, channelsPart) <- initChannelsPart chans
       return [ pingPart
              , nickUserPart
              , channelsPart
              , dicePart
              , helloPart
              ]


ircBotPlugin :: Plugin IrcBotURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig [TL.Text -> ClckT ClckURL IO TL.Text]
ircBotPlugin = Plugin
    { pluginName       = "ircBot"
    , pluginInit       = ircBotInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   =
         do dd <- liftIO getDataDir
            addPluginPath (pluginName ircBotPlugin) dd
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI ircBotPlugin
