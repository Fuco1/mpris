{-# LANGUAGE OverloadedStrings #-}

-- | Implements the org.mpris.MediaPlayer2 interface.
--
-- More information at <http://specifications.freedesktop.org/mpris-spec/latest/Media_Player.html>
module DBus.Mpris.MediaPlayer2
       ( module DBus.Mpris.MediaPlayer2.Methods
       , module DBus.Mpris.MediaPlayer2.Properties
       , module DBus.Mpris.MediaPlayer2.Data
       ) where

import DBus.Mpris.MediaPlayer2.Methods
import DBus.Mpris.MediaPlayer2.Properties
import DBus.Mpris.MediaPlayer2.Data
