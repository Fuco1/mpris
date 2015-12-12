{-# LANGUAGE OverloadedStrings #-}

module Mpris.Monad
       ( Mpris
       , State(..)
       , current
       , Mpris.Monad.call
       , runMpris
       ) where

import Control.Exception (bracket)

import Control.Monad.State hiding (State)

import DBus
import DBus.Client

data State = State { client :: Client
                   , players :: [BusName] }

type Mpris a = StateT State IO a

currentPlayer :: State -> BusName
currentPlayer = head . players

current :: Mpris BusName
current = gets currentPlayer

call :: MethodCall -> Mpris (Either MethodError MethodReturn)
call method = do
  client <- gets client
  liftIO $ DBus.Client.call client method

runMpris :: Mpris a -> IO a
runMpris code = bracket
  (do
   client <- connectSession
   return State { client = client, players = [busName_ "org.mpris.MediaPlayer2.mpd"] })
  (\(State {client = client}) -> disconnect client)
  (evalStateT code)
