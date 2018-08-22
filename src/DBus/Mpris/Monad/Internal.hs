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

listQueuedOwnersCall :: String -> MethodCall
listQueuedOwnersCall service = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListQueuedOwners")
  { methodCallDestination = Just "org.freedesktop.DBus"
  , methodCallBody = [toVariant service] }

-- | List all owners of the service
listQueuedOwners :: D.Client -> BusName -> IO (Maybe [String])
listQueuedOwners client service = fromVariant . head . methodReturnBody <$> D.call_ client (listQueuedOwnersCall $ formatBusName service)

nameHasOwnerCall :: String -> MethodCall
nameHasOwnerCall bus = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner")
  { methodCallDestination = Just "org.freedesktop.DBus"
  , methodCallBody = [toVariant bus] }

-- | Check if a bus is still alive.
nameHasOwner :: D.Client -> BusName -> IO Bool
nameHasOwner client bus =
  fromMaybe False . fromVariant . head . methodReturnBody <$> D.call_ client (nameHasOwnerCall $ formatBusName bus)

getNameOwnerCall :: String -> MethodCall
getNameOwnerCall bus = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "GetNameOwner")
  { methodCallDestination = Just "org.freedesktop.DBus"
  , methodCallBody = [toVariant bus] }

getNameOwner :: D.Client -> BusName -> IO BusName
getNameOwner client bus =
  fromMaybe "" . fromVariant . head . methodReturnBody <$> D.call_ client (getNameOwnerCall $ formatBusName bus)

-- | Get all available mpris-enabled players.
getPlayers :: D.Client -> IO [BusName]
getPlayers client = do
  buses <- listNames client
  case buses of
   Nothing -> return []
   Just b  -> do
     let mprisBusses = L.map busName_ . L.filter (isPrefixOf "org.mpris.MediaPlayer2.") $ b
     concat <$> mapM (\x -> listQueuedOwners client x >>= return . map busName_ . fromMaybe []) mprisBusses
