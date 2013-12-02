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
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )


\end{code}

<h2> The generic State for the Alarm system </h2>
+ [**Alarm**][alarm]
+ [**Call**][call]     
+ [**Count**][count]    
+ [**Person**][person]    
+ **timer**    


--------------------------------------------------

[call]:
**Call** represents the various States of the notification engine


-- |This is the fundamental internal state of the alarm system

\begin{code}


data Call  = NotCalling | Calling | Answered | NoAnswer | Ack  |NotAck
            deriving (Eq, Ord, Read, Show, Data, Typeable)                                   



\end{code}

------------------------------------------------

[person]:

**People** determines who the system should call, the input datatype should be
 a Set Style Monad (With Order)
 


\begin{code}


data Person p e cnt = Person  { phoneNumber :: p ,
                           email ::e,
                           callCount::cnt
                           }
            deriving (Eq, Ord, Read, Show, Data, Typeable)                                   

newtype People a = People a
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   




type DefaultPerson = Person Int Text Int
type DefaultPeople = People (Vector Text)


firstPerson :: People (Vector a) -> a
firstPerson (People a) = head a 
              

lastPerson :: People (Vector a) -> a
lastPerson (People a) = last a

\end{code}

--------------------------------------------------

[alarm]
**Alarm** handles the modes that an alarm as
 *Clear
 *Clearing
 *Tripped
 *Tripping


\begin{code}

data Alarm = Clear | Clearing | Tripped | Tripping
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   


\end{code}

 [count]

 **Count** is defined to simplify reading alarm counts in the statemachine.

\begin{code}


data Count = More | Max
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   



\end{code}


\begin{code}


data GraphState al cl cnt p  t = GraphState { 
       alarm :: al,
       call  :: cl,
       count :: cnt,
       person :: p,
       timer :: t
 
 }
   deriving (Eq, Ord, Read, Show, Data, Typeable)
            
type DefaultGraphState = GraphState Alarm Call Count DefaultPeople


\end{code}


<h2> Alarm Definition </h2>

**Alarms** are defined separately from the state of the program**

This is the information that is needed to fill out and check states.
It is immutable in the state machine and provided with external functions.

\begin{code}

data AlarmDefinition  i c t r cl = AlarmDefinition {
  alarmID   :: (AlarmId i),
  clearTime :: c,
  tripTime :: t,
  recallTime :: r,
  callList :: cl
  }
   deriving (Eq, Ord, Read, Show, Data, Typeable)                                   


newtype AlarmId i = AlarmId i
            deriving (Eq, Ord, Read, Show, Data, Typeable)                                   



type DefaultAlarmId = AlarmId Int
type DefaultAlarmDefinition = AlarmDefinition DefaultAlarmId Int Int Int ( DefaultPeople)

\end{code}


  

<h2> Acid State definition </h2>

**Acid State** is a pure haskell database solution
that helps keep this piece really portable

\begin{code}


$(deriveSafeCopy 0 'base ''GraphState )

\end{code}

