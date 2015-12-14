-- | Utilities for work with dbus which are missing from the dbus
-- backage.
module Mpris.Utils
       ( to
       ) where

import DBus

-- | Send a method call to a specific bus.
--
-- This just sets the 'methodCallDestination' property of 'MethodCall'
-- in a bit more convenient manner.
to :: MethodCall -> BusName -> MethodCall
method `to` bus = method { methodCallDestination = Just bus }
