module DBus.Mpris
       ( module DBus.Mpris.Monad
       , module DBus.Mpris.MediaPlayer2
       , module DBus.Mpris.MediaPlayer2.Player
       , getPlayer
       , getPlayers
       ) where

import DBus (BusName)
import DBus.Mpris.Monad
import DBus.Mpris.MediaPlayer2
import DBus.Mpris.MediaPlayer2.Player

import Control.Monad.State.Class

getPlayer :: BusName -> Mpris Player
getPlayer bus = do
  n <- identity bus
  p <- position bus
  v <- volume bus
  m <- metadata bus
  return $ Player bus n p v m

getPlayers :: Mpris [Player]
getPlayers = gets players >>= mapM getPlayer
