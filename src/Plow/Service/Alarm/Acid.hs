{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Plow.Service.Alarm.Acid where

import Plow.Service.Alarm.Types
import Data.Text hiding (head, last)
import Prelude   hiding (head, last)
-- import Data.Time
import Data.Vector
import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )


{-| Various naming conventions for stuff here I will try to stick to:

* Controller -> looks at the current state and decides what should happen based apon it

* Handler -> takes an incoming state and some parameters and returns  a new state

* Rxer  -> recieves some piece of data from an outside source

* Sender -> sends a piece of data to an outside source.

* Store -> put a piece of data into the local storage

* Get -> get a piece of data from the local storage

|-}



testPeople :: DefaultPeople
testPeople = People (fromList [Person 555 "test@Test.com" 0])



initialAlarmState :: AlarmState
initialAlarmState = AlarmState Clear NotCalling More testPeople 

defaultAlarmParameters :: AlarmParameters
defaultAlarmParameters = AlarmParameters 0 0 0 "test" testPeople

defaultAlarmRunner :: AlarmRunner
defaultAlarmRunner = AlarmRunner (AlarmId 0) defaultAlarmParameters initialAlarmState


initialAlarmTimer :: AlarmTimer
initialAlarmTimer = AlarmTimer (AlarmId 0 ) 0



-- | Cause I am dumb and will forget this... 
-- | Update is an instance of MonadState so you get a 'get' and 'put'


incTimer :: Update AlarmTimer Int
incTimer  = do c@AlarmTimer{..} <- get 
               let newTimer = succ timer
               return newTimer



-- | Reader instance on Query, allows ask

checkTimer :: Query AlarmTimer Int 
checkTimer = do 
  timer <$> ask

getSystemState :: Query AlarmTimer AlarmTimer
getSystemState = ask

-- | Definition of Acidic events 

$(makeAcidic ''AlarmTimer ['incTimer, 'checkTimer])

