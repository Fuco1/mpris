{-# LANGUAGE OverloadedStrings #-}

-- | Properties from the org.mpris.MediaPlayer2.Player interface
-- All properties are currently read-only
module Mpris.MediaPlayer2.Player.Properties
       ( PlaybackStatus
       , playbackStatus
       , LoopStatus
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

import Mpris.Properties
import Mpris.Monad

unpackIntM :: Mpris (Maybe Int64) -> Mpris (Maybe Integer)
unpackIntM = liftM . liftM $ fromIntegral

readM :: Read a => Mpris (Maybe String) -> Mpris (Maybe a)
readM = liftM . liftM $ read

property :: IsVariant a => String -> BusName -> Mpris (Maybe a)
property = getProperty "org.mpris.MediaPlayer2.Player"

-- | A playback state.
data PlaybackStatus = Playing -- ^ A track is currently playing.
                    | Paused  -- ^ A track is currently paused.
                    | Stopped -- ^ There is no track currently playing.
                    deriving (Show, Read)

-- | The current playback status.
playbackStatus :: BusName -> Mpris (Maybe PlaybackStatus)
playbackStatus = readM . property "PlaybackStatus"

-- | A repeat / loop status
data LoopStatus = None     -- ^ The playback will stop when there are no more tracks to play
                | Track    -- ^ The current track will start again from the begining once it has finished playing
                | Playlist -- ^ The playback loops through a list of tracks
                deriving (Show, Read)

-- | The current loop / repeat status
loopStatus :: BusName -> Mpris (Maybe LoopStatus)
loopStatus = readM . property "LoopStatus"

rate :: BusName -> Mpris (Maybe Double)
rate = property "Rate"

shuffle :: BusName -> Mpris (Maybe Bool)
shuffle = property "Shuffle"

metadata :: BusName -> Mpris (Maybe (Map String Variant))
metadata = property "Metadata"

volume :: BusName -> Mpris (Maybe Double)
volume = property "Volume"

position :: BusName -> Mpris (Maybe Integer)
position = unpackIntM . property "Position"

minimumRate :: BusName -> Mpris (Maybe Double)
minimumRate = property "MinimumRate"

maximumRate :: BusName -> Mpris (Maybe Double)
maximumRate = property "MaximumRate"

canGoNext :: BusName -> Mpris (Maybe Bool)
canGoNext = property "CanGoNext"

canGoPrevious :: BusName -> Mpris (Maybe Bool)
canGoPrevious = property "CanGoPrevious"

canPlay :: BusName -> Mpris (Maybe Bool)
canPlay = property "CanPlay"

canPause :: BusName -> Mpris (Maybe Bool)
canPause = property "CanPause"

canSeek :: BusName -> Mpris (Maybe Bool)
canSeek = property "CanSeek"

canControl :: BusName -> Mpris (Maybe Bool)
canControl = property "CanControl"
