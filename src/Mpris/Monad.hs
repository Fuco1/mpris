{-# LANGUAGE OverloadedStrings #-}

-- | Convenient monad for running MPRIS actions.
--
-- It is a 'StateT' on top of 'IO' which handles the connection and
-- caches available players as 'BusName's.  Inside it are a couple
-- useful functions to work with current player and make MPRIS calls.
module Mpris.Monad
       ( Mpris
       , State(..)
       , current
       , call
       , call_
       , runMpris
       ) where

import Control.Exception (bracket)
import Control.Monad (void)

import Control.Monad.State hiding (State)

import DBus
import qualified DBus.Client as D

-- | Internal state.
--
-- It holds the dbus 'client' which is used to make calls.
--
-- The list of 'players' works as a LRU stack, when a player changes
-- it is removed from the list and placed on top.
data State = State { client :: D.Client -- ^ The dbus client used to make the calls
                   , players :: [BusName] -- ^ List of available players-capable players as bus names
                   }

-- | Type wrapper for Mpris "monad".
type Mpris a = StateT State IO a

-- | Extract current player from the 'State'.
currentPlayer :: State -> BusName
currentPlayer = head . players

-- | Return current player
current :: Mpris BusName
current = gets currentPlayer

-- | Call a method call in context of current client
call :: MethodCall -> Mpris (Either MethodError MethodReturn)
call method = do
  client <- gets client
  liftIO $ D.call client method

-- | Like 'call' but ignores the result
call_ :: MethodCall -> Mpris ()
call_ = void . call

-- | Run the 'Mpris' computation and return the result inside 'IO'
runMpris :: Mpris a -> IO a
runMpris code = bracket
  (do
   client <- D.connectSession
   return State { client = client, players = [busName_ "org.mpris.MediaPlayer2.mpd"] })
  (\(State {client = client}) -> D.disconnect client)
  (evalStateT code)
