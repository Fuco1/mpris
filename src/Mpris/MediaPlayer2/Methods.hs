{-# LANGUAGE OverloadedStrings #-}

-- | Methods from the org.mpris.MediaPlayer2 interface
--
-- More information at <http://specifications.freedesktop.org/mpris-spec/latest/Media_Player.html>
module Mpris.MediaPlayer2.Methods
       ( quit
       , raise
       ) where

import DBus

import Mpris.Monad
import Mpris.Utils (to)

mprisCall :: MemberName -> MethodCall
mprisCall = methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2"

-- | Brings the media player's user interface to the front using any appropriate mechanism available.
raise :: BusName -> Mpris ()
raise bus = call_ (mprisCall "Raise" `to` bus)

-- | Cause the media player to stop running.
quit :: BusName -> Mpris ()
quit bus = call_ (mprisCall "Quit" `to` bus)
