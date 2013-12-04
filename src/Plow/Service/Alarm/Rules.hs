{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Plow.Service.Alarm.Rules where
import Data.Vector
import Plow.Service.Alarm.Types hiding (firstPerson, lastPerson)

checkAlarmState :: AlarmRunner -> AlarmState -> Bool
checkAlarmState (AlarmRunner _ ac s1)  s2 = True


data StateIterator a= StateIterator { 
      firstPerson :: a,
      nextPerson  :: a,
      lastPerson  :: a } deriving (Read,Eq,Show,Ord)
      

edgeStateChecks :: StateIterator Person -> AlarmState -> AlarmState -> Bool 
edgeStateChecks _ s1@(AlarmState Clear NotCalling More p1) s2@(AlarmState Tripping NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripping NotCalling More p1) s2@(AlarmState Clear NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripping NotCalling More p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ s1@(AlarmState Tripped Calling More p1) s2@(AlarmState Tripped Answered More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Calling More p1) s2@(AlarmState Tripped Answered Max p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Calling More p1) s2@(AlarmState Tripped NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Calling More p1) s2@(AlarmState Tripped NoAnswer Max p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Answered More p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ s1@(AlarmState Tripped Answered More p1) s2@(AlarmState Tripped Ack More p2)
  |p1 == p2 = True              
edgeStateChecks _ s1@(AlarmState Tripped Answered More p1) s2@(AlarmState Tripped NotAck More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Answered Max p1) s2@(AlarmState Tripped Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks _ s1@(AlarmState Tripped Answered Max p1) s2@(AlarmState Tripped NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped Answered Max p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks _ s1@(AlarmState Tripped NoAnswer More p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Tripped NoAnswer More p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks si s1@(AlarmState Tripped NoAnswer Max p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2
edgeStateChecks si s1@(AlarmState Tripped NoAnswer Max p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2                    
edgeStateChecks _ s1@(AlarmState Tripped NotAck More p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ s1@(AlarmState Tripped Ack More p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks si s1@(AlarmState Tripped NotAck Max p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si)  = (nextPerson si)  == p2
edgeStateChecks si s1@(AlarmState Tripped Ack Max p1) s2@(AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2

-- Clearing States
edgeStateChecks _ s1@(AlarmState Clearing Calling More p1) s2@(AlarmState Clearing Answered More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Clearing Calling More p1) s2@(AlarmState Clearing Answered Max p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Clearing Calling More p1) s2@(AlarmState Clearing NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Clearing Calling More p1) s2@(AlarmState Clearing NoAnswer Max p2)
  |p1 == p2 = True


-----

edgeStateChecks _ s1@(AlarmState Clearing Answered More p1) s2@(AlarmState Clearing Ack More p2)
  |p1 == p2 = True              
edgeStateChecks _ s1@(AlarmState Clearing Answered More p1) s2@(AlarmState Clearing NotAck More p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Clearing Answered Max p1) s2@(AlarmState Clearing Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks _ s1@(AlarmState Clearing Answered Max p1) s2@(AlarmState Clearing NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks _ s1@(AlarmState Clearing NoAnswer More p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == p2 = True
edgeStateChecks si s1@(AlarmState Clearing NoAnswer Max p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2
edgeStateChecks _ s1@(AlarmState Clearing NotAck More p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks si s1@(AlarmState Clearing NotAck Max p1) s2@(AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si)  = (nextPerson si)  == p2
edgeStateChecks _ s1@(AlarmState Clearing Ack More p1) s2@(AlarmState Clear NotCalling More p2) = True                    
edgeStateChecks _ s1@(AlarmState Clearing Ack Max p1) s2@(AlarmState Clear NotCalling More p2) = True
edgeStateChecks _ _ _ = False


