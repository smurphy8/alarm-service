{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Plow.Service.Alarm.Rules where
-- import Data.Vector
import Plow.Service.Alarm.Types hiding (firstPerson, lastPerson)

checkAlarmState :: AlarmRunner -> AlarmState -> Bool
checkAlarmState (AlarmRunner _ _ _)  _  = True


data StateIterator a= StateIterator { 
      firstPerson :: a,
      nextPerson  :: a,
      lastPerson  :: a } deriving (Read,Eq,Show,Ord)
      

edgeStateChecks :: StateIterator Person -> AlarmState -> AlarmState -> Bool 
edgeStateChecks _ (AlarmState Clear NotCalling More p1) (AlarmState Tripping NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripping NotCalling More p1) (AlarmState Clear NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripping NotCalling More p1) (AlarmState Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ (AlarmState Tripped Calling More p1) (AlarmState Tripped Answered More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Calling More p1) (AlarmState Tripped Answered Max p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Calling More p1) (AlarmState Tripped NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Calling More p1) (AlarmState Tripped NoAnswer Max p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Answered More p1) (AlarmState Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ (AlarmState Tripped Answered More p1) (AlarmState Tripped Ack More p2)
  |p1 == p2 = True              
edgeStateChecks _ (AlarmState Tripped Answered More p1) (AlarmState Tripped NotAck More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Answered Max p1) (AlarmState Tripped Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks _ (AlarmState Tripped Answered Max p1) (AlarmState Tripped NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped Answered Max p1) (AlarmState Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks _ (AlarmState Tripped NoAnswer More p1) (AlarmState Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Tripped NoAnswer More p1) (AlarmState Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks si (AlarmState Tripped NoAnswer Max p1) (AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2
edgeStateChecks si (AlarmState Tripped NoAnswer Max p1) (AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2                    
edgeStateChecks _ (AlarmState Tripped NotAck More p1) (AlarmState Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks _ (AlarmState Tripped Ack More p1) (AlarmState Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks si (AlarmState Tripped NotAck Max p1) (AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si)  = (nextPerson si)  == p2
edgeStateChecks si (AlarmState Tripped Ack Max p1) (AlarmState Tripped Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2

-- Clearing States
edgeStateChecks _ (AlarmState Clearing Calling More p1) (AlarmState Clearing Answered More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Clearing Calling More p1) (AlarmState Clearing Answered Max p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Clearing Calling More p1) (AlarmState Clearing NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Clearing Calling More p1) (AlarmState Clearing NoAnswer Max p2)
  |p1 == p2 = True


-----

edgeStateChecks _ (AlarmState Clearing Answered More p1) (AlarmState Clearing Ack More p2)
  |p1 == p2 = True              
edgeStateChecks _ (AlarmState Clearing Answered More p1) (AlarmState Clearing NotAck More p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Clearing Answered Max p1) (AlarmState Clearing Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks _ (AlarmState Clearing Answered Max p1) (AlarmState Clearing NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks _ (AlarmState Clearing NoAnswer More p1) (AlarmState Clearing Calling More p2)
  |p1 == p2 = True
edgeStateChecks si (AlarmState Clearing NoAnswer Max p1) (AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si)  && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si) = (nextPerson si) == p2
edgeStateChecks _ (AlarmState Clearing NotAck More p1) (AlarmState Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks si (AlarmState Clearing NotAck Max p1) (AlarmState Clearing Calling More p2)
  |p1 == (lastPerson si) && p2 == (firstPerson si) = True
  |p1 /= (lastPerson si)  = (nextPerson si)  == p2
edgeStateChecks _ (AlarmState Clearing Ack More _) (AlarmState Clear NotCalling More _) = True                    
edgeStateChecks _ (AlarmState Clearing Ack Max _)(AlarmState Clear NotCalling More _) = True
edgeStateChecks _ _ _ = False


