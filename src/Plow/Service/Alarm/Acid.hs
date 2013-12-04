cab{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Plow.Service.Alarm.Acid where
import Plow.Service.Alarm.Rules
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

person1 :: Person 
person1 = Person 555 "test@Test.com" 0

testPeople :: DefaultPeople
testPeople = People (fromList [Person 555 "test@Test.com" 0])



initialAlarmState :: AlarmState
initialAlarmState = AlarmState Clear NotCalling More person1

defaultAlarmParameters :: AlarmParameters
defaultAlarmParameters = AlarmParameters 0 0 0 "test" testPeople

defaultAlarmRunner :: AlarmRunner
defaultAlarmRunner = AlarmRunner (AlarmId 0) defaultAlarmParameters initialAlarmState


initialAlarmTimer :: AlarmTimer
initialAlarmTimer = AlarmTimer (AlarmId 0 ) 0

{-| 
timer incrementor, checker and reset |-}


-- | increment the timer by i, flexible alarm incrementor for 
-- | time stallouts and resuming after crashes

incTimer :: Int -> Update AlarmTimer Int
incTimer  i = do c@AlarmTimer{..} <- get 
                 let newTimer = timer + i
                 return newTimer


-- | Peek at the time in an alarm to update states
-- | Non-blocking guarantees 

checkTimer :: Query AlarmTimer Int 
checkTimer = do 
  timer <$> ask

resetTimer :: Update AlarmTimer Int 
resetTimer = do c@AlarmTimer{..} <- get 
                return 0



{-| AlarmState, 
 * 'Alarm' Mutator, Checker
 * 
 * 'Call' Mutator, Checker
 * 'Person'
   + Incrementor
   + Reset
 * 'Count' Mutator, Checker
|-}



changeAlarmState :: AlarmState -> Update AlarmRunner AlarmState
changeAlarmState a = do c@AlarmRunner{..} <- get 
                   case checkAlarmState c a of 
                     True -> return a
                     False -> return alarmState
                      

-- | Definition of Acidic events 

$(makeAcidic ''AlarmRunner ['changeAlarmState])
$(makeAcidic ''AlarmTimer ['incTimer, 'checkTimer, 'resetTimer])


