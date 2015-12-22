import DBus
import DBus.Mpris.Monad
import DBus.Mpris.MediaPlayer2.Player

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default

myPlaybackStatusHook :: BusName -> Maybe PlaybackStatus -> Mpris ()
myPlaybackStatusHook bus status =
  liftIO . print $ "Status changed " ++ show bus ++ " to " ++ show status

myLoopStatusHook :: BusName -> Maybe LoopStatus -> Mpris ()
myLoopStatusHook bus status =
  liftIO . print $ "Loop changed " ++ show bus ++ " to " ++ show status

myVolumeHook :: BusName -> Maybe Double -> Mpris ()
myVolumeHook bus status =
  liftIO . print $ "Volume changed " ++ show bus ++ " to " ++ show status

config :: Config
config = def { playbackStatusHook = myPlaybackStatusHook
             , loopStatusHook = myLoopStatusHook

             , volumeHook = myVolumeHook
             }

main = mpris config (liftIO $ forever $ threadDelay 800000)
