module Plow.Service.Alarm.Types where


data State al cl cnt p = ST { 
       alarm :: al,
       call  :: cl,
       count :: cnt,
       person :: p 
 
 }
        deriving (Eq,Ord)
