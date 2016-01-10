{-# LANGUAGE OverloadedStrings #-}

-- | Internal helpers for the Mpris monad
module DBus.Mpris.Monad.Internal
       ( getPlayers
       , nameHasOwner
       ) where

import Control.Applicative ((<$>))
import Data.List as L
import Data.Maybe (fromMaybe)

import DBus
import qualified DBus.Client as D

-- | A call listing all available dbus buses.
listNamesCall :: MethodCall
listNamesCall = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
  { methodCallDestination = Just "org.freedesktop.DBus" }

-- | List all available dbus buses as strings
listNames :: D.Client -> IO (Maybe [String])
listNames client = fromVariant . head . methodReturnBody <$> D.call_ client listNamesCall

nameHasOwnerCall :: String -> MethodCall
nameHasOwnerCall bus = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner")
  { methodCallDestination = Just "org.freedesktop.DBus"
  , methodCallBody = [toVariant bus] }

-- | Check if a bus is still alive.
nameHasOwner :: D.Client -> BusName -> IO Bool
nameHasOwner client bus =
  fromMaybe False . fromVariant . head . methodReturnBody <$> D.call_ client (nameHasOwnerCall $ formatBusName bus)

-- | Get all available mpris-enabled players.
getPlayers :: D.Client -> IO [BusName]
getPlayers client = do
  buses <- listNames client
  return $ case buses of
   Nothing -> []
   Just b  -> L.map busName_ . L.filter (\x ->
     all ($x) [ (/= "org.mpris.MediaPlayer2.vlc")
              , isPrefixOf "org.mpris.MediaPlayer2."]) $ b
