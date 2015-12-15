{-# LANGUAGE OverloadedStrings #-}

-- | Implements the methods of org.mpris.MediaPlayer2.Player interface.
--
-- More information at <http://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html>
module DBus.Mpris.MediaPlayer2.Player.Methods
       ( next
       , previous
       , pause
       , playPause
       , stop
       , play
       , seek
       , setPosition
       , openUri
       ) where

import Data.Int
import DBus

import DBus.Mpris.Monad
import DBus.Mpris.Utils

mprisCall :: MemberName -> MethodCall
mprisCall = methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"

-- | Skips to the next track in the tracklist.
next :: BusName -> Mpris ()
next bus = call_ $ mprisCall "Next" `to` bus

-- | Skips to the previous track in the tracklist.
previous :: BusName -> Mpris ()
previous bus = call_ $ mprisCall "Previous" `to` bus

-- | Pauses playback.
pause :: BusName -> Mpris ()
pause bus = call_ $ mprisCall "Pause" `to` bus

-- | Pauses playback, if playback is already paused, resumes playback.
playPause :: BusName -> Mpris ()
playPause bus = call_ $ mprisCall "PlayPause" `to` bus

-- | Stops playback.
stop :: BusName -> Mpris ()
stop bus = call_ $ mprisCall "Stop" `to` bus

-- | Starts or resumes playback.
play :: BusName -> Mpris ()
play bus = call_ $ mprisCall "Play" `to` bus

-- | Seeks forward in the current track by the specified number of microseconds.
seek :: Integer -- ^ The number of microseconds to seek forward.
     -> BusName -- ^ Bus
     -> Mpris ()
seek position bus = call_ $ (mprisCall "Seek")
  { methodCallBody = [toVariant (fromIntegral position :: Int64)] } `to` bus

-- | Sets the current track position in microseconds.
setPosition :: ObjectPath -- ^ The currently playing track's identifier.
            -> Integer    -- ^ Track position in microseconds.
            -> BusName    -- ^ Bus
            -> Mpris ()
setPosition object position bus = call_ $ (mprisCall "SetPosition")
  { methodCallBody = [ toVariant (fromIntegral position :: Int64)
                     , toVariant object
                     ] } `to` bus

-- | Opens the Uri given as an argument
openUri :: String  -- ^ Uri of the track to load.
        -> BusName -- ^ Bus
        -> Mpris ()
openUri uri bus = call_ $ (mprisCall "OpenUri")
  { methodCallBody = [toVariant uri] } `to` bus
