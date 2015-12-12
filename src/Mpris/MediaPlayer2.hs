{-# LANGUAGE OverloadedStrings #-}

module Mpris.MediaPlayer2
       ( quit
       , identity
       ) where

import DBus
import Control.Monad.State hiding (State)

import Mpris.Monad
import Mpris.Utils

mprisCall :: MemberName -> MethodCall
mprisCall = methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2"

quitCall :: MethodCall
quitCall = mprisCall "Quit"

-- presunut do Properties, pricom do MediaPlayer2.Properties pojde verzia s vyplnenym "cielom"
propertyCall :: String -> MethodCall
propertyCall property = (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "Get")
  { methodCallBody = [ toVariant ("org.mpris.MediaPlayer2" :: String)
                     , toVariant property ] }

-- | Cause the current media player to stop running.
quit :: Mpris ()
quit = current >>= quitPlayer

-- | Cause the media player to stop running.
quitPlayer :: BusName -> Mpris ()
quitPlayer bus = do
  let c = quitCall { methodCallDestination = Just bus }
  call c
  return ()

identity :: Mpris (Maybe String)
identity = current >>= identityPlayer

identityPlayer :: BusName -> Mpris (Maybe String)
identityPlayer bus = do
  let c = propertyCall "Identity" `to` bus
  reply <- call c
  liftIO $ print reply
  case reply of
    Left _   -> return Nothing
    Right r -> do
      let body = methodReturnBody r
          name = fromVariant (head body) >>= fromVariant
      return name
