> {-# LANGUAGE BangPatterns,RankNTypes #-}

> module Main where
> import Prelude 
> import Control.Monad
> import Control.Applicative

-- import Data.GraphViz

> import Data.Graph.Inductive.Graphviz
> import Data.Graph.Inductive.Graph
> import Data.Graph.Inductive.PatriciaTree
> import Data.List 
> import System.IO (writeFile)

> main :: IO () 
> main = do 
>  print "hello"



> data People = Judy | Jackie |Bobby | Sue
>              deriving (Enum,Eq,Show,Ord,Bounded)
> data Alarm = Clear | Clearing | Tripped |Tripping 
>              deriving (Enum,Eq,Show,Ord)
> 
> data Call = NotCalling | Calling | Answered | NoAnswer | Ack  |NotAck
>              deriving (Enum,Eq,Show,Ord)

> data Count = More | Max 
>            deriving (Enum,Eq,Show,Ord)

> data State al cl cnt p = ST { 
>       alarm :: al,
>       call  :: cl,
>       count :: cnt,
>       person :: p 
> 
> }
>        deriving (Eq,Ord)

> instance (Show a,Show c,Show cnt,Show p) => Show (State a c cnt p ) where
>     show s = showGraphState s

True is an allowable state change

> alarmStateChecks :: Alarm -> Alarm -> Bool
> alarmStateChecks  Clear Clearing =  False
> alarmStateChecks  Clear Tripped  =  False
> alarmStateChecks  Clear Tripping =  True
> alarmStateChecks  Tripped Clearing  =  True
> alarmStateChecks  Tripped Tripping = False 
> alarmStateChecks  Tripped Clear  = False
> alarmStateChecks  Tripping Clearing = False 
> alarmStateChecks  Tripping Clear = True 
> alarmStateChecks  Tripping Tripped = True 
> alarmStateChecks  Clearing Clear = True
> alarmStateChecks  Clearing Tripped = True 
> alarmStateChecks  Clearing Tripping = False
> alarmStateChecks  _  _ = True

> alarmStateChecks' :: GraphState -> GraphState -> Bool
> alarmStateChecks' (ST Tripping NotCalling _ _) (ST Tripped c _ _) 
>  |c == Calling = True
>  | otherwise = False
> alarmStateChecks' _ _ = True
    


> callStateChecks :: Call -> Call -> Bool 
> callStateChecks NotCalling Calling = True 
> callStateChecks NotCalling NotCalling = True 
> callStateChecks NotCalling _ = False
> callStateChecks Calling NotCalling = False
> callStateChecks Calling _ = True 
> -- any state beside calling or not calling is now in first underscore
> callStateChecks _ Calling = True 
> callStateChecks _ NotCalling = True 
> callStateChecks _ _ = False



---------------------------------------------------------

> countStateChecks :: Count -> Count -> Bool 
> countStateChecks Max Max = False --State change must occur
> countStateChecks _ _ = True
> 
> countStateChecks' :: GraphState -> GraphState -> Bool 
> countStateChecks' (ST _ c1 Max p1) (ST _ c2 ncnt p2) 
>     |(p1 == p2) = False -- State change must occur
>     |otherwise = True
> countStateChecks' (ST _ c1 More p1) (ST _ c2 More p2) 
>     |(p1 == p2) = True -- State change must not occur
>     |otherwise = False 
> countStateChecks' (ST _ c1 More p1) (ST _ c2 Max p2) 
>     |(c1 == Calling || c1 == NotAck) && (p1 == p2) = True 
>     | otherwise = False
> 
> countStateChecks' _ _ = True

---------------------------------------------------------

> peopleStateChecks :: People -> People -> Bool 
> peopleStateChecks p1 p2 
>     |p1 == p2 = True
>     |p1 == maxBound = p2 == minBound                       
>     |(succ p1) == p2 = True 
>     |otherwise = False
> 
> -- | The number of primes in the edgeState determines the number of dependencies
> edgeStateChecks :: GraphState -> GraphState -> Bool 
> edgeStateChecks s1@(ST a1 c1 cnt1 p1) s2@(ST a2 c2 cnt2 p2) 
>                 |alarmStateChecks a1 a2 && 
>                   callStateChecks c1 c2 && 
>                   countStateChecks cnt1 cnt2 &&
>                   peopleStateChecks p1 p2 = edgeStateChecks' s1 s2
>                 |otherwise = False
> 
> edgeStateChecks' s1@(ST a1 c1 cnt1 p1) s2@(ST a2 c2 cnt2 p2)      
>     |alarmStateChecks' s1 s2 && countStateChecks' s1 s2 = True
>     |otherwise = False
> 
> 
> onlyPossibleEdges :: [AlarmEdge]
> onlyPossibleEdges = do 
>   n1@(i1,s1) <- onlyPossibleNodes
>   n2@(i2,s2) <- onlyPossibleNodes
>   guard (i1 /= i2)
>   guard $ edgeStateChecks s1 s2
>   return (i1,i2,"")
    



> showGraphState (ST a c cnt p) = show a  ++ "," ++ show c ++ "," ++ show cnt ++ "," ++ show p

> printGraphState (ST a c cnt p) = putStrLn $ show a  ++ " " ++ show c ++ " " ++ show cnt ++ " " ++ show p
> type GraphState = State Alarm Call Count People



-- | State Lists

> walkThrough :: (Enum a) => Int ->  a -> [a]
> walkThrough n = (take n).( iterate succ)

> allPeople :: [People]
> allPeople = walkThrough 4 Judy

> allAlarms :: [Alarm]
> allAlarms = walkThrough 4 Clear
> 
> allCalls :: [Call]
> allCalls = walkThrough 6 NotCalling
> 
> allCounts :: [Count]
> allCounts = walkThrough 2 More

-- | State N-tuples 

> allStates :: [GraphState]
> allStates = do 
>   p <- allPeople
>              
>   a <- allAlarms
>              
>   c <- allCalls 
>        
>   cnt <- allCounts
>   return $ ST  a c cnt p



> onlyPossibleStates :: [GraphState]
> onlyPossibleStates = do 
>   s <- allStates 
>   guard (not $ alarm s == Tripped && call s == NotCalling)
>   guard (not $ alarm s == Tripping && call s /= NotCalling)
>   guard (not $ alarm s == Clear && call s /= NotCalling)
>   guard (not $ alarm s == Clear && person s /= minBound)
>   guard (not $ alarm s == Clearing && call s == NotCalling)
>   guard (not $ count s == Max && alarm s == Clear )
>   guard (not $ count s == Max && alarm s == Tripping )
>   guard (not $ count s == Max && call s == Calling)
> 
>   return $ s


> allEdgeTuples :: [(GraphState,GraphState)] 
> allEdgeTuples = do 
>   s1 <- onlyPossibleStates
>   s2 <- onlyPossibleStates
> --  guard $ s1 /= s2 
>   return $ (s1,s2)




> makeStateNodes :: [GraphState] -> [(Node,GraphState)]
> makeStateNodes st = zip [1 ..] st
> 
> allEdges :: [(Node, Node)]
> allEdges = do 
>    (i1,_) <- makeStateNodes onlyPossibleStates
>    (i2,_) <- makeStateNodes onlyPossibleStates
>    guard (i1 /= i2)
>    return (i1,i2)


> type AlarmNode = (Node,GraphState)
> type AlarmEdge = (Node,Node,String)
> onlyPossibleNodes :: [AlarmNode]
> onlyPossibleNodes = makeStateNodes onlyPossibleStates
> 
> stateGraph :: Gr (GraphState) String
> stateGraph = mkGraph onlyPossibleNodes  onlyPossibleEdges
> 
> writeGraph = writeFile "./diagrams/graph.gv" $ graphviz stateGraph "fgl" (8.0,11.0) (1,1) Portrait
