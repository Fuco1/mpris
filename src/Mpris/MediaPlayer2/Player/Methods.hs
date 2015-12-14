{-# LANGUAGE OverloadedStrings #-}

module Mpris.MediaPlayer2.Player.Methods
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

import Mpris.Monad
import Mpris.Utils

mprisCall :: MemberName -> MethodCall
mprisCall = methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"

next :: BusName -> Mpris ()
next bus = call_ $ mprisCall "Next" `to` bus

previous :: BusName -> Mpris ()
previous bus = call_ $ mprisCall "Previous" `to` bus

pause :: BusName -> Mpris ()
pause bus = call_ $ mprisCall "Pause" `to` bus

playPause :: BusName -> Mpris ()
playPause bus = call_ $ mprisCall "PlayPause" `to` bus

stop :: BusName -> Mpris ()
stop bus = call_ $ mprisCall "Stop" `to` bus

play :: BusName -> Mpris ()
play bus = call_ $ mprisCall "Play" `to` bus

seek :: Integer -> BusName -> Mpris ()
seek position bus = call_ $ (mprisCall "Seek")
  { methodCallBody = [toVariant (fromIntegral position :: Int64)] } `to` bus

setPosition :: ObjectPath -> Integer -> BusName -> Mpris ()
setPosition object position bus = call_ $ (mprisCall "SetPosition")
  { methodCallBody = [ toVariant (fromIntegral position :: Int64)
                     , toVariant object
                     ] } `to` bus

openUri :: String -> BusName -> Mpris ()
openUri uri bus = call_ $ (mprisCall "OpenUri")
  { methodCallBody = [toVariant uri] } `to` bus
