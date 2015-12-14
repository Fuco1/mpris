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

-- | Report if the player can quit.  If 'False', calling 'quit' will have no effect.
canQuit :: BusName -> Mpris (Maybe Bool)
canQuit = property "CanQuit"

-- | Whether the media player is occupying the fullscreen.
fullscreen :: BusName -> Mpris (Maybe Bool)
fullscreen = property "Fullscreen"

-- | If 'False', attempting to set Fullscreen will have no effect.
canSetFullscreen :: BusName -> Mpris (Maybe Bool)
canSetFullscreen = property "CanSetFullscreen"

-- | If 'False', calling 'raise' will have no effect.
canRaise :: BusName -> Mpris (Maybe Bool)
canRaise = property "CanRaise"

-- | Indicates whether the /org/mpris/MediaPlayer2 object implements the org.mpris.MediaPlayer2.TrackList interface.
hasTrackList :: BusName -> Mpris (Maybe Bool)
hasTrackList = property "HasTrackList"

-- | A friendly name to identify the media player to users.
identity :: BusName -> Mpris (Maybe String)
identity = property "Identity"

-- | The basename of an installed .desktop file which complies with the Desktop entry specification, with the ".desktop" extension stripped.
desktopEntry :: BusName -> Mpris (Maybe String)
desktopEntry = property "DesktopEntry"
