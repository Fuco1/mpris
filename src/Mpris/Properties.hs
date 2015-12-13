{-# LANGUAGE OverloadedStrings #-}

module Mpris.Properties
       ( getProperty
       ) where

import DBus

import Mpris.Monad
import Mpris.Utils

-- | Construct a call to get value of property at interface
propertyCall :: String     -- ^ Interface
             -> String     -- ^ Property name
             -> MethodCall
propertyCall interface property = (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "Get")
  { methodCallBody = [toVariant interface, toVariant property] }

-- | Get value of interface's property at bus
getProperty :: IsVariant a =>
               String  -- ^ Interface
            -> String  -- ^ Property
            -> BusName -- ^ Bus
            -> Mpris (Maybe a)
getProperty interface prop bus = do
  reply <- call $ propertyCall interface prop `to` bus
  case reply of
    Left _   -> return Nothing
    Right r -> do
      let body = methodReturnBody r
      -- the bind here runs inside Maybe monad, neat!
      return $ fromVariant (head body) >>= fromVariant
