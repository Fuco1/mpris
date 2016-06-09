import DBus
import DBus.Mpris.Monad
import DBus.Mpris.MediaPlayer2.Player

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust a f = maybe (return ()) f a

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM a f = a >>= \v -> whenJust v f

myPlaybackStatusHook :: Callback PlaybackStatus
myPlaybackStatusHook = do
  bus <- bus
  whenJustM value $ \status ->
    liftIO . print $ "Status changed " ++ show bus ++ " to " ++ show status

myLoopStatusHook :: Callback LoopStatus
myLoopStatusHook = do
  bus <- bus
  whenJustM value $ \status ->
    liftIO . print $ "Loop changed " ++ show bus ++ " to " ++ show status

myVolumeHook :: Callback Double
myVolumeHook = do
  bus <- bus
  whenJustM value $ \volume ->
    liftIO . print $ "Volume changed " ++ show bus ++ " to " ++ show volume

config :: Config
config = def { playbackStatusHook = myPlaybackStatusHook
             , loopStatusHook = myLoopStatusHook

             , volumeHook = myVolumeHook
             }

main = mpris config (liftIO $ forever $ threadDelay 800000)
