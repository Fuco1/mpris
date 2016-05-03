{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- To evaluate a Mpris action use 'mpris' which brings it back into
-- 'IO', for example:
--
-- > main :: IO ()
-- > main = do
-- >   let bus = busName_ "org.mpris.MediaPlayer2.mpd"
-- >   mpris def $ pause bus
--
-- There is also a low-level function 'runMpris' where you have to
-- provide the 'State' manually.
module DBus.Mpris.Monad
       ( Mpris
       , State(..)
       , Config(..)
       , Event(..)
       , Callback
       , Call
       , bus
       , value
       , liftMpris
       , call
       , call_
       , isAlive
       , runMpris
       , forkMpris
       , logMessage
       , mpris
       ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Default
import Data.IORef
import qualified Data.Map as M
import qualified Data.List as L

import DBus
import qualified DBus.Client as D

import DBus.Mpris.Monad.Internal
import DBus.Mpris.Monad.Data
import DBus.Mpris.MediaPlayer2.Player.Data

data Config = Config
  { playbackStatusHook :: Callback PlaybackStatus
  , loopStatusHook :: Callback LoopStatus
  , volumeHook :: Callback Double
  , playerQuitHook :: Callback ()
  , logger :: String -> IO ()
  }

newtype Call a b = Call (ReaderT (BusName, Maybe a) Mpris b)
               deriving (Functor, Monad, MonadIO, MonadReader (BusName, Maybe a))

instance Monoid b => Monoid (Call a b) where
  mempty = Call $ return mempty
  Call f `mappend` Call g = Call $ f >> g

liftMpris :: Mpris b -> Call a b
liftMpris = Call . lift

bus :: Call a BusName
bus = Call $ fst `fmap` ask

value :: Call a (Maybe a)
value = Call $ snd `fmap` ask

type Callback a = Call a ()

runCallback :: Callback a -> BusName -> Maybe a -> Mpris ()
runCallback (Call callback) bus value = runReaderT callback (bus, value)

updateCurrentPlayer :: Callback PlaybackStatus
updateCurrentPlayer = do
  bus <- bus
  value <- value
  liftMpris $ case value of
    Just Playing -> modify (\state@State { players = players } ->
                             state { players = update players bus } )
    _            -> return ()
  where update players new = new : (L.delete new players)

deletePlayer :: Callback ()
deletePlayer = do
  bus <- bus
  liftMpris $ modify (\s@State { players = pl } ->
                       s { players = L.delete bus pl })

instance Default Config where
  def = Config { playbackStatusHook = updateCurrentPlayer
               , loopStatusHook = mempty
               , volumeHook = mempty
               , playerQuitHook = deletePlayer
               , logger = print }

-- | Internal state.
--
-- It holds the dbus 'client' which is used to make calls.
--
-- The list of 'players' works as a LRU stack, when a player changes
-- it is removed from the list and placed on top.
data State = State { client :: D.Client -- ^ The dbus client used to make the calls
                   , players :: [BusName] -- ^ List of available players-capable players as bus names
                   , chan :: Chan Event -- ^ Event channel
                   , currentInitialized :: Bool -- ^ A flag signifying whether the current player has been initialized.
                   }

-- | Type wrapper for Mpris monad
newtype Mpris a = Mpris { unMpris :: RWST Config () (IORef State) IO a }
                deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadState State Mpris where
  get = Mpris $ get >>= liftIO . readIORef
  put s = Mpris $ get >>= liftIO . flip atomicWriteIORef s

-- | Call a method call in context of current dbus client.
call :: MethodCall -> Mpris (Either D.ClientError (Either MethodError MethodReturn))
call method = do
  client <- gets client
  liftIO $ try (D.call client method >>= evaluate)

-- | Like 'call' but ignores the result.
call_ :: MethodCall -> Mpris ()
call_ = void . call

-- | Check if a bus is still alive.
isAlive :: BusName -> Mpris Bool
isAlive bus = do
  client <- gets client
  liftIO $ nameHasOwner client bus

-- | PropertiesChanged signal matcher.
mprisEventMatcher :: D.MatchRule
mprisEventMatcher = D.matchAny
  { D.matchSender = Nothing
  , D.matchDestination = Nothing
  , D.matchPath = Just "/org/mpris/MediaPlayer2"
  , D.matchInterface = Just "org.freedesktop.DBus.Properties"
  , D.matchMember = Just "PropertiesChanged"
  }

-- TODO: move the matchers somewhere else
-- |
nameOwnerChangedEventMatcher :: D.MatchRule
nameOwnerChangedEventMatcher = D.matchAny
  { D.matchSender = Nothing
  , D.matchDestination = Nothing
  , D.matchPath = Just "/org/freedesktop/DBus"
  , D.matchInterface = Just "org.freedesktop.DBus"
  , D.matchMember = Just "NameOwnerChanged"
  }

nameOwnerChangedCallback :: Chan Event -> Signal -> IO ()
nameOwnerChangedCallback chan s = do
  let [origin', _, new'] = signalBody s
      origin = fromVariant origin'
      new = fromVariant new' :: Maybe String
  case (origin, new) of
   (Just o@(':':_), Just "") -> writeChan chan (PlayerQuit (busName_ o) (Just ()))
   (_, _) -> return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m a = maybe (return ()) a m

-- | PropertiesChanged callback.
propertiesChangedCallback :: Chan Event -> Signal -> IO ()
propertiesChangedCallback chan s = do
  let bus = signalSender s
      dict = fromVariant (signalBody s !! 1) :: Maybe (M.Map String Variant)
  case (bus, dict) of
    (Just bus, Just dict) -> do
      whenJust (M.lookup "PlaybackStatus" dict)
        (writeChan chan . PlaybackStatusChanged bus . fmap read . fromVariant)
      whenJust (M.lookup "LoopStatus" dict)
         (writeChan chan . LoopStatusChanged bus . fmap read . fromVariant)
      whenJust (M.lookup "Volume" dict)
         (writeChan chan . VolumeChanged bus . fromVariant)
    (_, _) -> return ()

processEvent :: Event -> Mpris ()
processEvent event = do
  config <- ask
  case event of
    PlaybackStatusChanged bus status ->
      runCallback (playbackStatusHook config) bus status
    LoopStatusChanged bus status ->
      runCallback (loopStatusHook config) bus status
    VolumeChanged bus volume ->
      runCallback (volumeHook config) bus volume
    PlayerQuit bus v -> do
      pl <- gets players
      when (bus `elem` pl) $ runCallback (playerQuitHook config) bus v

-- | Main event loop
eventLoop :: Chan Event -> Mpris ()
eventLoop chan = forever $ do
    event <- liftIO $ readChan chan
    processEvent event

-- | Run the 'Mpris' computation and return the result inside 'IO'.
runMpris :: Mpris a -> Config -> IORef State -> IO a
runMpris code config state = fst `fmap` evalRWST (unMpris code) config state

forkMpris :: Mpris () -> Mpris ()
forkMpris action = do
  config <- ask
  state <- Mpris get
  void . liftIO $ forkIO $ runMpris action config state

-- | Log a message using the logger given in the config.
logMessage :: Show a => a -> Mpris ()
logMessage s = do
  l <- logger `fmap` ask
  liftIO $ l (show s)

-- | Connect to the bus, initialize the state and run 'Mpris' computation.
mpris :: Config -> Mpris a -> IO a
mpris config code = bracket
  (do
   client <- D.connectSession
   players <- getPlayers client
   chan <- newChan
   D.addMatch client mprisEventMatcher (propertiesChangedCallback chan)
   D.addMatch client nameOwnerChangedEventMatcher (nameOwnerChangedCallback chan)
   newIORef State {
     client = client
   , players = players
   , chan = chan
   , currentInitialized = False })
  (\state -> do
    State { client = client } <- readIORef state
    logger config "Exiting..."
    D.disconnect client)
  (\state -> do
    State { chan = chan } <- readIORef state
    forkIO $ runMpris (eventLoop chan) config state
    runMpris code config state)
