{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.IrcBot.URL where

import Data.Data     (Data, Typeable)
import Web.Routes.TH (derivePathInfo)

data IrcBotAdminURL
    = IrcBotReconnect
    | IrcBotSettings
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''IrcBotAdminURL)

data IrcBotURL
    = IrcLogs
    | IrcLog FilePath
    | IrcBotAdmin IrcBotAdminURL
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''IrcBotURL)
