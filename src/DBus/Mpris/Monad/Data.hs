module DBus.Mpris.Monad.Data
       ( Event(..)
       ) where

import DBus
import DBus.Mpris.MediaPlayer2.Player.Data

-- | Mpris events
data Event =
    PlaybackStatusChanged BusName (Maybe PlaybackStatus)
  | LoopStatusChanged BusName (Maybe LoopStatus)
  | VolumeChanged BusName (Maybe Double)
  deriving (Show)
