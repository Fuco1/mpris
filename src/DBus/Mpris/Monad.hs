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
       , Config
       , current
       , call
       , call_
       , runMpris
       ) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad.RWS
import Data.Default
import Data.IORef

import DBus
import qualified DBus.Client as D

import DBus.Mpris.Monad.Internal (getPlayers)
import DBus.Mpris.MediaPlayer2.Player.Data

data Config = Config
  { playbackStatusHook :: BusName -> Maybe PlaybackStatus -> Mpris ()
  , volumeHook :: BusName -> Maybe Double -> Mpris ()
  }

instance Default Config where
  def = Config { playbackStatusHook = empty
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

-- | Run the 'Mpris' computation and return the result inside 'IO'.
runMpris :: Config -> Mpris a -> IO a
runMpris config code = bracket
  (do
   client <- D.connectSession
   players <- getPlayers client
   -- register the property listener
   -- D.addMatch client mprisEventMatcher (propertiesChangedCallback client)
   newIORef State { client = client, players = players }
  )
  (\state -> do
    State {client = client} <- readIORef state
    D.disconnect client)
  (liftM fst . evalRWST (unMpris code) config)
