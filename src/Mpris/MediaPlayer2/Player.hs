{-# LANGUAGE OverloadedStrings #-}

module Mpris.MediaPlayer2.Player
       ( playPause
       ) where

import DBus
import Control.Monad.State hiding (State)

import Mpris.Monad
import Mpris.Utils

mprisCall :: MemberName -> MethodCall
mprisCall = methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"

playPauseCall :: MethodCall
playPauseCall = mprisCall "PlayPause"

-- TODO: should return the resulting state: paused/playing
playPause :: Mpris ()
playPause = current >>= playPausePlayer

playPausePlayer :: BusName -> Mpris ()
playPausePlayer bus = do
  let c = playPauseCall `to` bus
  call c
  return ()
