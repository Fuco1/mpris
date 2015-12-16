{-# LANGUAGE OverloadedStrings #-}

-- | Internal helpers for the Mpris monad
module DBus.Mpris.Monad.Internal
       ( getPlayers
       ) where

import Control.Applicative ((<$>))
import Data.List as L

import DBus
import qualified DBus.Client as D

-- | A call listing all available dbus buses.
listNamesCall :: MethodCall
listNamesCall = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
  { methodCallDestination = Just "org.freedesktop.DBus" }

-- | List all available dbus buses as strings
listNames :: D.Client -> IO (Maybe [String])
listNames client = fromVariant . head . methodReturnBody <$> D.call_ client listNamesCall

-- | Get all available mpris-enabled players.
getPlayers :: D.Client -> IO [BusName]
getPlayers client = do
  buses <- listNames client
  return $ case buses of
   Nothing -> []
   Just b  -> L.map busName_ . L.filter (\x ->
     all ($x) [ (/= "org.mpris.MediaPlayer2.vlc")
              , isPrefixOf "org.mpris.MediaPlayer2."]) $ b
