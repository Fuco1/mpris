-- | Implements the org.mpris.MediaPlayer2.Player interface.
--
-- More information at <http://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html>
module DBus.Mpris.MediaPlayer2.Player
       ( current
       , module DBus.Mpris.MediaPlayer2.Player.Methods
       , module DBus.Mpris.MediaPlayer2.Player.Properties
       , module DBus.Mpris.MediaPlayer2.Player.Data
       ) where

import Control.Monad.RWS
import Data.Maybe
import Data.List
import DBus
import DBus.Mpris.Monad
import DBus.Mpris.MediaPlayer2.Player.Methods
import DBus.Mpris.MediaPlayer2.Player.Properties
import DBus.Mpris.MediaPlayer2.Player.Data

-- | Extract current player from the 'State'.
currentPlayer :: State -> Maybe BusName
currentPlayer = listToMaybe . players

-- | Return current player
current :: Mpris (Maybe BusName)
current = do
  c' <- gets currentPlayer
  case c' of
    Just c -> do
      alive <- isAlive c
      if alive
        then do
          isInit <- gets currentInitialized
          if isInit
            then return $ Just c
            else initializeCurrentPlayer
        else do
          modify (\s@State { players = p } ->
                   s { players = tail p } )
          current
    Nothing -> return Nothing

initializeCurrentPlayer :: Mpris (Maybe BusName)
initializeCurrentPlayer = do
  p <- gets players
  pls <- mapM (\x -> do { ps <- playbackStatus x ; return (ps, x) }) p
  let active = listToMaybe [ x | (Just Playing, x) <- pls ]
      new = case active of
              Just a -> a : delete a (map snd pls)
              Nothing -> map snd pls
  modify (\s -> s { players = new, currentInitialized = True })
  return active
