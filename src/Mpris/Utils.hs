module Mpris.Utils
       ( to
       ) where

import DBus

to :: MethodCall -> BusName -> MethodCall
method `to` bus = method { methodCallDestination = Just bus }
