<link rel="stylesheet" type="text/css" href="bootstrap.css">

<div class="page-header">
  <h1> Alarm Types <small> State machine definition </small </h1>  </div>




\begin{code}

{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
module Plow.Service.Alarm.Types where

import Data.Text hiding (head, last)
import Prelude   hiding (head, last)
import Data.Vector

\end{code}

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
               deriving (Enum,Eq,Show,Ord)


\end{code}

------------------------------------------------

<a name="person"/>

**People** determines who the system should call, the input datatype should be
 a Set Style Monad (With Order)
 


\begin{code}


data Person p e cnt = Person  { phoneNumber :: p ,
                           email ::e,
                           callCount::cnt
                           }
            deriving (Eq,Show)

newtype People a = People a
                 deriving (Eq,Show)



type DefaultPerson = Person Int Text Int
type DefaultPeople = People (Vector Text)


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
           deriving (Eq,Show)

\end{code}

<a name="count"/>

 **Count** is defined to simplify reading alarm counts in the statemachine.

\begin{code}


data Count = More | Max 
            deriving (Enum,Eq,Show,Ord)


\end{code}


\begin{code}


data State al cl cnt p  t = ST { 
       alarm :: al,
       call  :: cl,
       count :: cnt,
       person :: p,
       timer :: t
 
 }
   deriving (Eq,Ord)


type DefaultGraphState = State Alarm Call Count DefaultPeople


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
  deriving (Show, Eq)

newtype AlarmId i = AlarmId i
                deriving (Show, Eq)

type DefaultAlarmId = AlarmId Int
type DefaultAlarmDefinition = AlarmDefinition DefaultAlarmId Int Int Int ( DefaultPeople)

\end{code}


  

