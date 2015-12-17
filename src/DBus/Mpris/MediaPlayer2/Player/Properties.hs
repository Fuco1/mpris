{-# LANGUAGE OverloadedStrings #-}

-- | Properties from the org.mpris.MediaPlayer2.Player interface
--
-- All properties are currently read-only
--
-- More information at <http://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html>
module DBus.Mpris.MediaPlayer2.Player.Properties
       ( PlaybackStatus(..)
       , playbackStatus
       , LoopStatus(..)
       , loopStatus
       , rate
       , shuffle
       , metadata
       , volume
       , position
       , minimumRate
       , maximumRate
       , canGoNext
       , canGoPrevious
       , canPlay
       , canPause
       , canSeek
       , canControl
       ) where

import DBus
import Data.Map
import Data.Int (Int64)
import Control.Monad (liftM)

import DBus.Mpris.Properties
import DBus.Mpris.Monad

import DBus.Mpris.MediaPlayer2.Player.Data

unpackIntM :: Mpris (Maybe Int64) -> Mpris (Maybe Integer)
unpackIntM = liftM . liftM $ fromIntegral

readM :: Read a => Mpris (Maybe String) -> Mpris (Maybe a)
readM = liftM . liftM $ read

property :: IsVariant a => String -> BusName -> Mpris (Maybe a)
property = getProperty "org.mpris.MediaPlayer2.Player"

-- | The current playback status.
playbackStatus :: BusName -> Mpris (Maybe PlaybackStatus)
playbackStatus = readM . property "PlaybackStatus"

-- | The current loop / repeat status
loopStatus :: BusName -> Mpris (Maybe LoopStatus)
loopStatus = readM . property "LoopStatus"

-- | The current playback rate.
rate :: BusName -> Mpris (Maybe Double)
rate = property "Rate"

-- | A value of 'False' indicates that playback is progressing
-- linearly through a playlist, while 'True' means playback is
-- progressing through a playlist in some other order.
shuffle :: BusName -> Mpris (Maybe Bool)
shuffle = property "Shuffle"

-- | The metadata of the current element.
metadata :: BusName -> Mpris (Maybe (Map String Variant))
metadata = property "Metadata"

-- | The volume level.
volume :: BusName -> Mpris (Maybe Double)
volume = property "Volume"

-- | The current track position in microseconds, between 0 and the
-- 'mpris:length' metadata entry (see Metadata).
position :: BusName -> Mpris (Maybe Integer)
position = unpackIntM . property "Position"

-- | The minimum value which the Rate property can take.
minimumRate :: BusName -> Mpris (Maybe Double)
minimumRate = property "MinimumRate"

-- | The maximum value which the Rate property can take.
maximumRate :: BusName -> Mpris (Maybe Double)
maximumRate = property "MaximumRate"

-- | Whether the client can call the 'next' method on this interface and
-- expect the current track to change.
canGoNext :: BusName -> Mpris (Maybe Bool)
canGoNext = property "CanGoNext"

-- | Whether the client can call the 'previous' method on this interface
-- and expect the current track to change.
canGoPrevious :: BusName -> Mpris (Maybe Bool)
canGoPrevious = property "CanGoPrevious"

-- | Whether playback can be started using 'play' or 'playPause'.
canPlay :: BusName -> Mpris (Maybe Bool)
canPlay = property "CanPlay"

-- | Whether playback can be paused using 'pause' or 'playPause. '
canPause :: BusName -> Mpris (Maybe Bool)
canPause = property "CanPause"

-- | Whether the client can control the playback position using 'seek'
-- and 'setPosition'.
canSeek :: BusName -> Mpris (Maybe Bool)
canSeek = property "CanSeek"

-- | Whether the media player may be controlled over this interface.
canControl :: BusName -> Mpris (Maybe Bool)
canControl = property "CanControl"
