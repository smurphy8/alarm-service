<link rel="stylesheet" type="text/css" href="bootstrap.css">

<div class="page-header">
  <h1> Alarm Types <small> State machine definition </small </h1>  </div>




\begin{code}

{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Plow.Service.Alarm.Types where

import Data.Text hiding (head, last)
import Prelude   hiding (head, last)
import Data.Vector
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )


\end{code}

[Use SafeCopy Version 0.8.3 or greater](https://github.com/acid-state/safecopy.git)


<h2> The generic State for the Alarm system </h2>
+ [**Alarm**](#alarm)
+ [**Call**](#call)
+ [**Count**](#count)
+ [**Person**](#person)
+ **timer**    


--------------------------------------------------

<a name="call"/>
**Call** represents the various States of the notification engine


-- |This is the fundamental internal state of the alarm system

\begin{code}


data Call  = NotCalling | Calling | Answered | NoAnswer | Ack  |NotAck
            deriving (Eq, Ord, Read, Show, Data, Typeable)                                   

\end{code}

------------------------------------------------

<a name="person"/>

**People** determines who the system should call, the input datatype should be
 a Set Style Monad (With Order)
 


\begin{code}


data Person = Person  { phoneNumber :: Int ,
                           email ::Text,
                           callCount::Int
                           }
            deriving (Eq, Ord, Read, Show, Data, Typeable)                                   

newtype People a = People a
   deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy)                                   





type DefaultPeople = People (Vector (Person))


firstPerson :: People (Vector a) -> a
firstPerson (People a) = head a 
              

lastPerson :: People (Vector a) -> a
lastPerson (People a) = last a

\end{code}

--------------------------------------------------

<a name="alarm"/>
**Alarm** handles the modes that an alarm as
 *Clear
 *Clearing
 *Tripped
 *Tripping


\begin{code}

data Alarm = Clear | Clearing | Tripped | Tripping
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   


\end{code}

<a name="count"/>

 **Count** is defined to simplify reading alarm counts in the statemachine.

\begin{code}


data Count = More | Max
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   



\end{code}


\begin{code}


data AlarmState = AlarmState { 
       alarm :: Alarm,
       call  :: Call,
       count :: Count,
       person::Person
 
 }
   deriving (Eq, Ord, Read, Show, Data, Typeable)
            



\end{code}


<h2> Alarm Definition </h2>

**Alarms** are defined separately from the state of the program**
This is the information that is needed to fill out and check states.
It is immutable in the state machine and provided with external functions.

\begin{code}

data AlarmParameters = AlarmParameters {
  clearTime :: Int,
  tripTime :: Int,
  recallTime :: Int,
  message :: Text,
  callList :: People (Vector (Person))
  }
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   


\end{code}

--------------------------------------------------


<h2> AlarmRunner Definition </h2> 

The Alarm Runner pairs an internal state ('AlarmState') 
with an (immutable) configuration ('AlarmParameters').

The 'timer' is kept separate from the alarm definition. 

This is so that it can run continuously and be 'peeked' at by the state handler.

\begin{code}


newtype AlarmId i = AlarmId i
            deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy)                                   

type DefaultAlarmId = AlarmId Int

data AlarmRunner = AlarmRunner {
      alarmID   :: (AlarmId Int),
      alarmParameters :: AlarmParameters,
      alarmState :: AlarmState

    }
 deriving (Eq, Ord, Show, Data, Typeable)

data AlarmTimer = AlarmTimer { 
      alarmTimerID   :: (AlarmId Int),
      timer :: Int
}
 deriving (Eq, Ord, Show, Data, Typeable)



\end{code}

<h2> Acid State definition </h2>

**Acid State** is a pure haskell database solution
that helps keep this piece really portable

\begin{code}



$(deriveSafeCopy 0 'base ''AlarmParameters)

$(deriveSafeCopy 0 'base ''Person)

$(deriveSafeCopy 0 'base ''Call)

$(deriveSafeCopy 0 'base ''Alarm)

$(deriveSafeCopy 0 'base ''Count)

$(deriveSafeCopy 0 'base ''AlarmState )

$(deriveSafeCopy 0 'base ''AlarmRunner)

$(deriveSafeCopy 0 'base ''AlarmTimer)
\end{code}

