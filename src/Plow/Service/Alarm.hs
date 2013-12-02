module Plow.Service.Alarm where

-- import Plow.Service.Alarm.Internal

import Plow.Service.Alarm.Types



{-| Various naming conventions for stuff here I will try to stick to:

Controller -> looks at the current state and decides what should happen based apon it

Handler -> takes an incoming state and some parameters and returns  a new state

Rxer  -> recieves some piece of data from an outside source

Sender -> sends a piece of data to an outside source.

Store -> put a piece of data into the local storage

Get -> get a piece of data from the local storage

|-}



