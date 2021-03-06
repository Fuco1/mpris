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
       , Metadata(..)
       , metadata
       , volume
       , setVolume
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
import Data.Word (Word64)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import Prelude hiding (lookup)

import qualified DBus.Mpris.Properties as P
import DBus.Mpris.Monad

import DBus.Mpris.MediaPlayer2.Player.Data as Data

unpackIntM :: Mpris (Maybe Int64) -> Mpris (Maybe Integer)
unpackIntM = liftM . liftM $ fromIntegral

readM :: Read a => Mpris (Maybe String) -> Mpris (Maybe a)
readM = liftM . liftM $ read

property :: IsVariant a => String -> BusName -> Mpris (Maybe a)
property = P.getProperty "org.mpris.MediaPlayer2.Player"

setProperty :: IsVariant a => String -> BusName -> a -> Mpris ()
setProperty = P.setProperty "org.mpris.MediaPlayer2.Player"

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
metadata :: BusName -> Mpris (Maybe Metadata)
metadata bus = fmap (\md -> Metadata {
    trackId = lookup "mpris:trackid" md >>= fromVariant
  , len = fmap fromInt (lookup "mpris:length" md >>= fromVariant) <|>
          fmap fromWord (lookup "mpris:length" md >>= fromVariant)
  , album = lookup "xesam:album" md >>= fromVariant
  , artist = head <$> (lookup "xesam:artist" md >>= fromVariant)
  , title = lookup "xesam:title" md >>= fromVariant
  , url = lookup "xesam:url" md >>= fromVariant
  , unknown = md
  }) `liftM` property "Metadata" bus
  where fromInt :: Int64 -> Integer
        fromInt = fromIntegral
        fromWord :: Word64 -> Integer
        fromWord = fromIntegral

-- | The volume level.
volume :: BusName -> Mpris (Maybe Double)
volume = property "Volume"

-- | Set volume level.
setVolume :: BusName -> Double -> Mpris ()
setVolume = setProperty "Volume"

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
