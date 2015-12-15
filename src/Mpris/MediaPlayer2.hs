{-# LANGUAGE OverloadedStrings #-}

-- | Implements the org.mpris.MediaPlayer2 interface.
--
-- More information at http://specifications.freedesktop.org/mpris-spec/latest/Media_Player.html
module Mpris.MediaPlayer2
       ( module Mpris.MediaPlayer2.Methods
       , module Mpris.MediaPlayer2.Properties
       ) where

import Mpris.MediaPlayer2.Methods
import Mpris.MediaPlayer2.Properties
