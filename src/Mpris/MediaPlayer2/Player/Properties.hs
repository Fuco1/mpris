{-# LANGUAGE OverloadedStrings #-}

-- | Properties from the org.mpris.MediaPlayer2.Player interface
-- All properties are currently read-only
module Mpris.MediaPlayer2.Player.Properties
       ( playbackStatus
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

property :: IsVariant a => String -> BusName -> Mpris (Maybe a)
property = getProperty "org.mpris.MediaPlayer2.Player"

playbackStatus :: BusName -> Mpris (Maybe String)
playbackStatus = property "PlaybackStatus"

loopStatus :: BusName -> Mpris (Maybe String)
loopStatus = property "LoopStatus"

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
