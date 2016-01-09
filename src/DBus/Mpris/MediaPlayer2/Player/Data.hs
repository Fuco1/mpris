module DBus.Mpris.MediaPlayer2.Player.Data
       ( PlaybackStatus(..)
       , LoopStatus(..)
       , Metadata(..)
       ) where

import Data.Map
import DBus (Variant)

-- | A playback state.
data PlaybackStatus = Playing -- ^ A track is currently playing.
                    | Paused  -- ^ A track is currently paused.
                    | Stopped -- ^ There is no track currently playing.
                    deriving (Eq, Show, Read)

-- | A repeat / loop status
data LoopStatus = None     -- ^ The playback will stop when there are no more tracks to play
                | Track    -- ^ The current track will start again from the begining once it has finished playing
                | Playlist -- ^ The playback loops through a list of tracks
                deriving (Eq, Show, Read)

-- In case we want to add more fields in the future, here's the list
-- Mpris properties:
--     mpris:trackid
--     mpris:length
--     mpris:artUrl
--
-- Common Xesam properties:
--
--     xesam:album
--     xesam:albumArtist
--     xesam:artist
--     xesam:asText
--     xesam:audioBPM
--     xesam:autoRating
--     xesam:comment
--     xesam:composer
--     xesam:contentCreated
--     xesam:discNumber
--     xesam:firstUsed
--     xesam:genre
--     xesam:lastUsed
--     xesam:lyricist
--     xesam:title
--     xesam:trackNumber
--     xesam:url
--     xesam:useCount
--     xesam:userRating

-- | Metadata about the current track
data Metadata = Metadata
  { trackId :: Maybe String -- ^ Mpris Track ID
  , len :: Maybe Integer    -- ^ Length in microseconds
  , album :: Maybe String   -- ^ Album
  , artist :: Maybe String  -- ^ Artist
  , title :: Maybe String   -- ^ Title
  , url :: Maybe String     -- ^ Url
  , unknown :: Map String Variant -- ^ Unknown fields
  } deriving (Show)
