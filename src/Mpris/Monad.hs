{-# LANGUAGE OverloadedStrings #-}

module Mpris.Monad
       ( Mpris
       , State(..)
       , current
       , call
       , runMpris
       ) where

import Control.Exception (bracket)

import Control.Monad.State hiding (State)

import DBus
import qualified DBus.Client as D

data State = State { client :: D.Client
                   , players :: [BusName] }

type Mpris a = StateT State IO a

currentPlayer :: State -> BusName
currentPlayer = head . players

current :: Mpris BusName
current = gets currentPlayer

call :: MethodCall -> Mpris (Either MethodError MethodReturn)
call method = do
  client <- gets client
  liftIO $ D.call client method

runMpris :: Mpris a -> IO a
runMpris code = bracket
  (do
   client <- D.connectSession
   return State { client = client, players = [busName_ "org.mpris.MediaPlayer2.mpd"] })
  (\(State {client = client}) -> D.disconnect client)
  (evalStateT code)
