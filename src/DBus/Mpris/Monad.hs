{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Convenient monad for running MPRIS actions.
--
-- It is a 'StateT' on top of 'IO' which handles the connection and
-- caches available players as 'BusName' s.
--
-- It contains some useful functions to work with current player and
-- make MPRIS calls.
--
-- For example, to pause the playback on current player, you can do
--
-- > pauseCurrent :: Mpris ()
-- > pauseCurrent = current >>= pause
--
-- To evaluate a Mpris action use 'runMpris' which brings it back into 'IO', for example:
--
-- > main :: IO ()
-- > main = do
-- >   let bus = busName_ "org.mpris.MediaPlayer2.mpd"
-- >   runMpris $ pause bus
module DBus.Mpris.Monad
       ( Mpris
       , State(..)
       , Config(..)
       , Event(..)
       , current
       , call
       , call_
       , runMpris
       , mpris
       ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Exception (bracket)
import Control.Monad.RWS
import Data.Default
import Data.IORef
import qualified Data.Map as M

import DBus
import qualified DBus.Client as D

import DBus.Mpris.Monad.Internal (getPlayers)
import DBus.Mpris.Monad.Data
import DBus.Mpris.MediaPlayer2.Player.Data

data Config = Config
  { playbackStatusHook :: BusName -> Maybe PlaybackStatus -> Mpris ()
  , loopStatusHook :: BusName -> Maybe LoopStatus -> Mpris ()
  , volumeHook :: BusName -> Maybe Double -> Mpris ()
  }

instance Default Config where
  def = Config { playbackStatusHook = empty
               , loopStatusHook = empty
               , volumeHook = empty }
    where empty _ _ = return ()

-- | Internal state.
--
-- It holds the dbus 'client' which is used to make calls.
--
-- The list of 'players' works as a LRU stack, when a player changes
-- it is removed from the list and placed on top.
data State = State { client :: D.Client -- ^ The dbus client used to make the calls
                   , players :: [BusName] -- ^ List of available players-capable players as bus names
                   , chan :: Chan Event -- ^ Event channel
                   }

-- | Type wrapper for Mpris monad
newtype Mpris a = Mpris { unMpris :: RWST Config () (IORef State) IO a } deriving Functor

instance Applicative Mpris where
  pure = Mpris . pure
  Mpris a <*> Mpris f = Mpris (a <*> f)

instance Monad Mpris where
  return = Mpris . return
  Mpris a >>= f = Mpris $ a >>= unMpris . f

instance MonadReader Config Mpris where
  ask = Mpris ask
  reader f = Mpris $ f `liftM` ask
  local f (Mpris a) = Mpris $ local f a

instance MonadState State Mpris where
  get = Mpris $ get >>= liftIO . readIORef
  put s = Mpris $ get >>= liftIO . flip atomicWriteIORef s

instance MonadIO Mpris where
  liftIO = Mpris . liftIO

-- | Extract current player from the 'State'.
currentPlayer :: State -> BusName
currentPlayer = head . players

-- | Return current player
current :: Mpris BusName
current = gets currentPlayer

-- | Call a method call in context of current dbus client.
call :: MethodCall -> Mpris (Either MethodError MethodReturn)
call method = do
  client <- gets client
  liftIO $ D.call client method

-- | Like 'call' but ignores the result.
call_ :: MethodCall -> Mpris ()
call_ = void . call

-- | PropertiesChanged signal matcher.
mprisEventMatcher :: D.MatchRule
mprisEventMatcher = D.matchAny
  { D.matchSender = Nothing
  , D.matchDestination = Nothing
  , D.matchPath = Just "/org/mpris/MediaPlayer2"
  , D.matchInterface = Just "org.freedesktop.DBus.Properties"
  , D.matchMember = Just "PropertiesChanged"
  }


-- | PropertiesChanged callback.
propertiesChangedCallback :: Chan Event -> Signal -> IO ()
propertiesChangedCallback chan s =
  case signalSender s of
    Just bus ->
      case fromVariant (signalBody s !! 1) :: Maybe (M.Map String Variant) of
        Just dict -> do
          case M.lookup "PlaybackStatus" dict of
            Just value -> writeChan chan
              (PlaybackStatusChanged bus ((liftM read . fromVariant) value))
            Nothing -> return ()
          case M.lookup "LoopStatus" dict of
            Just value -> writeChan chan
              (LoopStatusChanged bus ((liftM read . fromVariant) value))
            Nothing -> return ()
          case M.lookup "Volume" dict of
            Just value -> writeChan chan
              (VolumeChanged bus (fromVariant value))
            Nothing -> return ()
        Nothing -> return ()
    Nothing -> return ()

processEvent :: Event -> Mpris ()
processEvent event = do
  config <- ask
  case event of
    PlaybackStatusChanged bus status ->
      playbackStatusHook config bus status
    LoopStatusChanged bus status ->
      loopStatusHook config bus status
    VolumeChanged bus volume ->
      volumeHook config bus volume

-- | Main event loop
eventLoop :: Chan Event -> Mpris ()
eventLoop chan = forever $ do
    event <- liftIO $ readChan chan
    processEvent event

-- | Run the 'Mpris' computation and return the result inside 'IO'.
runMpris :: Mpris a -> Config -> IORef State -> IO a
runMpris code config state = fst `liftM` evalRWST (unMpris code) config state

-- | Connect to the bus, initialize the state and run 'Mpris' computation.
mpris :: Config -> Mpris a -> IO a
mpris config code = bracket
  (do
   client <- D.connectSession
   players <- getPlayers client
   chan <- newChan
   D.addMatch client mprisEventMatcher (propertiesChangedCallback chan)
   newIORef State {
     client = client
   , players = players
   , chan = chan })
  (\state -> do
    State { client = client } <- readIORef state
    print "Exiting..."
    D.disconnect client)
  (\state -> do
    State { chan = chan } <- readIORef state
    forkIO $ runMpris (eventLoop chan) config state
    runMpris code config state)
