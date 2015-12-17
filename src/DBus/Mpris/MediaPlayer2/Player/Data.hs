module DBus.Mpris.MediaPlayer2.Player.Data
       ( PlaybackStatus(..)
       , LoopStatus(..)
       ) where

-- | A playback state.
data PlaybackStatus = Playing -- ^ A track is currently playing.
                    | Paused  -- ^ A track is currently paused.
                    | Stopped -- ^ There is no track currently playing.
                    deriving (Show, Read)

-- | A repeat / loop status
data LoopStatus = None     -- ^ The playback will stop when there are no more tracks to play
                | Track    -- ^ The current track will start again from the begining once it has finished playing
                | Playlist -- ^ The playback loops through a list of tracks
                deriving (Show, Read)
