module DBus.Mpris.MediaPlayer2.Data
       ( Player(..)
       ) where

import DBus.Mpris.MediaPlayer2.Player.Data
import DBus (BusName)

data Player = Player { playerBusname :: BusName
                     , playerName :: Maybe String
                     , playerPosition :: Maybe Integer
                     , playerVolume :: Maybe Double
                     , playerMetadata :: Maybe Metadata
                     } deriving (Show)
