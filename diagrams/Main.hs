{-# LANGUAGE BangPatterns,RankNTypes #-}
module Main where
import Prelude 
import Control.Monad
import Control.Applicative
-- import Data.GraphViz
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List 
import System.IO (writeFile)

main :: IO () 
main = do 
  print "hello"





data People = Judy | Jackie |Bobby | Sue
             deriving (Enum,Eq,Show,Ord)


data Alarm = Clear | Clearing | Tripped |Tripping 
             deriving (Enum,Eq,Show,Ord)

data Call = NotCalling | Calling | Answered | NoAnswer | Ack  |NotAck
             deriving (Enum,Eq,Show,Ord)

data Count = More | Max 
           deriving (Enum,Eq,Show,Ord)

data State al cl cnt p = ST { 
      alarm :: al,
      call  :: cl,
      count :: cnt,
      person :: p 

}
       deriving (Eq,Show,Ord)


    



showGraphState (ST a c cnt p) = show a  ++ " " ++ show c ++ " " ++ show cnt ++ " " ++ show p

printGraphState (ST a c cnt p) = putStrLn $ show a  ++ " " ++ show c ++ " " ++ show cnt ++ " " ++ show p
type GraphState = State Alarm Call Count People



-- | State Lists
walkThrough :: (Enum a) => Int ->  a -> [a]
walkThrough n = (take n).( iterate succ)

allPeople :: [People]
allPeople = walkThrough 4 Judy

allAlarms :: [Alarm]
allAlarms = walkThrough 4 Clear

allCalls :: [Call]
allCalls = walkThrough 6 NotCalling

allCounts :: [Count]
allCounts = walkThrough 2 More

-- | State N-tuples 

allStates :: [GraphState]
allStates = do 
  p <- allPeople
             
  a <- allAlarms
             
  c <- allCalls 
       
  cnt <- allCounts
  return $ ST  a c cnt p



onlyPossibleStates :: [GraphState]
onlyPossibleStates = do 
  s <- allStates 
  guard (not $ alarm s == Tripping && call s /= NotCalling)
  guard (not $ alarm s == Clear && call s /= NotCalling)
  guard (not $ alarm s == Clearing && call s == NotCalling)
  return $ s


allEdgeTuples :: [(GraphState,GraphState)] 
allEdgeTuples = do 
  s1 <- onlyPossibleStates
  s2 <- onlyPossibleStates
  guard $ s1 /= s2 
  return $ (s1,s2)




makeStateNodes :: [GraphState] -> [(Node,GraphState)]
makeStateNodes st = zip [1 ..] st

allEdges :: [(Node, Node)]
allEdges = do 
   (i1,_) <- makeStateNodes onlyPossibleStates
   (i2,_) <- makeStateNodes onlyPossibleStates
   guard (i1 /= i2)
   return (i1,i2)


type AlarmNode = (Node,GraphState)
type AlarmEdge = (Node,Node,String)
onlyPossibleNodes :: [AlarmNode]
onlyPossibleNodes = makeStateNodes onlyPossibleStates

onlyPossibleEdges :: [AlarmEdge]
onlyPossibleEdges = do 
  n1@(i1,s1) <- onlyPossibleNodes
  n2@(i2,s2) <- onlyPossibleNodes
  return (i1,i2,"")

stateGraph :: Gr (GraphState) String
stateGraph = mkGraph onlyPossibleNodes (take 10 onlyPossibleEdges)

writeGraph = writeFile "./diagrams/graph.gv" $ graphviz' stateGraph