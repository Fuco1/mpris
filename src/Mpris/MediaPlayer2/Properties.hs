{-# LANGUAGE OverloadedStrings #-}

-- | Properties from the org.mpris.MediaPlayer2 interface
module Mpris.MediaPlayer2.Properties
       ( canQuit
       , fullscreen
       , identity
       , canSetFullscreen
       , canRaise
       , hasTrackList
       , desktopEntry
       ) where

import DBus

import Mpris.Properties
import Mpris.Monad

property :: IsVariant a => String -> BusName -> Mpris (Maybe a)
property = getProperty "org.mpris.MediaPlayer2"

canQuit :: BusName -> Mpris (Maybe Bool)
canQuit = property "CanQuit"

fullscreen :: BusName -> Mpris (Maybe Bool)
fullscreen = property "Fullscreen"

canSetFullscreen :: BusName -> Mpris (Maybe Bool)
canSetFullscreen = property "CanSetFullscreen"

canRaise :: BusName -> Mpris (Maybe Bool)
canRaise = property "CanRaise"

hasTrackList :: BusName -> Mpris (Maybe Bool)
hasTrackList = property "HasTrackList"

identity :: BusName -> Mpris (Maybe String)
identity = property "Identity"

desktopEntry :: BusName -> Mpris (Maybe String)
desktopEntry = property "DesktopEntry"
