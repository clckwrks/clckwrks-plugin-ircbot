{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.IrcBot.Page.IrcLogs where

import Control.Applicative   ((<$>))
import Control.Monad.Reader  (ask)
import Clckwrks
import Clckwrks.IrcBot.Monad
import Clckwrks.IrcBot.Page.Template (template)
import Clckwrks.IrcBot.URL
import Data.List (sort)
import System.Directory
import System.FilePath
import HSP.XMLGenerator
import HSP.XML
import Happstack.Server.HSP.HTML

ircLogs :: IrcBotM Response
ircLogs =
    do logDir   <- ircBotLogDirectory <$> ask
       logFiles <- liftIO $ (reverse . sort . filter ((\ext -> (ext == ".html") || (ext == ".txt")) . takeExtension)) <$> getDirectoryContents logDir
       urls     <- mapM (showURL . IrcLog) logFiles
       let logs = zip urls logFiles
       template "irc logs" ()
         <%>
          <h1>IRC Logs</h1>
          <ul>
           <% mapM (\(url, logFile) -> <li><a href=url><% logFile %></a></li>) logs %>
          </ul>
         </%>

