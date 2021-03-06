
<link rel="stylesheet" type="text/css" href="bootstrap.css">



<div class="page-header"> 
 <h1> Producing a graph from haskell 
 <small> A KISS Tutorial </small> </h1> </div>



---
<h2>[Produces This Graph](autograph.gv)</h2>
<h2> Which I use to make [This image](autograph.svg)  </h2>
---


\begin{code}
{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}

module Main where
import Prelude 
import Control.Monad
import Control.Applicative

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.Graphviz
import Data.IntSet

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List 

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.PrettyPrint hiding (Style)
import System.IO (writeFile)
\end{code}



\begin{code}
main :: IO () 
main = do 
  writeGraphViz
\end{code}


Sample Graph Nodes

These can be easily abstracted from here

\begin{code}
data People = First | Second |Third
              deriving (Enum,Eq,Show,Ord,Bounded)
\end{code}

\begin{code}
data Alarm = Clear | Clearing | Tripped |Tripping 
              deriving (Enum,Eq,Show,Ord)

\end{code}


\begin{code}
data Call = NotCalling | Calling | Answered | NoAnswer | Ack  |NotAck
              deriving (Enum,Eq,Show,Ord)

data Count = More | Max 
            deriving (Enum,Eq,Show,Ord)

type GraphState = State Alarm Call Count People


data State al cl cnt p = ST { 
       alarm :: al,
       call  :: cl,
       count :: cnt,
       person :: p 
 
 }
        deriving (Eq,Ord)

\end{code}


First, the States that make no sense are removed


\begin{code}

onlyPossibleStates :: [GraphState]
onlyPossibleStates = do 
  s <- allStates 
  guard (not $ alarm s == Tripping && person s /= minBound)
  guard (not $ alarm s == Tripped && call s == NotCalling)
  guard (not $ alarm s == Tripping && call s /= NotCalling)
  guard (not $ alarm s == Clear && call s /= NotCalling)
  guard (not $ alarm s == Clear && person s /= minBound)
  guard (not $ alarm s == Clearing && call s == NotCalling)
  guard (not $ count s == Max && alarm s == Clear )
  guard (not $ count s == Max && alarm s == Tripping )
  guard (not $ count s == Max && call s == Calling)
  return $ s



onlyPossibleNodes :: [AlarmNode]
onlyPossibleNodes = makeStateNodes onlyPossibleStates

\end{code}



A little handling for the DotCode type... See the

[DotCode Documentation](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Printing.html#t:DotCode)
for more info...


\begin{code}


instance (Show a,Show c,Show cnt,Show p) => Show (State a c cnt p ) where
     show s = showGraphState s




stToDot :: (Show a, Show c, Show cnt, Show  p ) => State a c cnt p -> DotCode
stToDot s@(ST a c cnt p ) = dotText.T.pack.show $ s

instance (Show a,Show c,Show cnt,Show p) =>PrintDot (State a c cnt p) where 
      unqtDot s@(ST a c cnt p) = stToDot s
      toDot s@(ST a c cnt p) = stToDot s
      unqtListToDot sLst =  unqtListToDot $ renderDot.unqtDot <$> sLst
      listToDot sLst = listToDot (renderDot.toDot <$> sLst)



\end{code}

Below is the Edge State Check, this is the heart of the state machine alarm.
States are locked in Calling modes and mutable in Not Calling, Answered and Tripping Modes

\begin{code}

edgeStateChecks :: GraphState -> GraphState -> Bool 
edgeStateChecks s1@(ST Clear NotCalling More p1) s2@(ST Tripping NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripping NotCalling More p1) s2@(ST Clear NotCalling More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripping NotCalling More p1) s2@(ST Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks s1@(ST Tripped Calling More p1) s2@(ST Tripped Answered More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Calling More p1) s2@(ST Tripped Answered Max p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Calling More p1) s2@(ST Tripped NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Calling More p1) s2@(ST Tripped NoAnswer Max p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Answered More p1) s2@(ST Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks s1@(ST Tripped Answered More p1) s2@(ST Tripped Ack More p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Tripped Answered More p1) s2@(ST Tripped NotAck More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Answered Max p1) s2@(ST Tripped Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Tripped Answered Max p1) s2@(ST Tripped NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped Answered Max p1) s2@(ST Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Tripped NoAnswer More p1) s2@(ST Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped NoAnswer More p1) s2@(ST Clearing Calling More p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Tripped NoAnswer Max p1) s2@(ST Tripped Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2
edgeStateChecks s1@(ST Tripped NoAnswer Max p1) s2@(ST Clearing Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2                    
edgeStateChecks s1@(ST Tripped NotAck More p1) s2@(ST Tripped Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks s1@(ST Tripped Ack More p1) s2@(ST Tripped Calling More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Tripped NotAck Max p1) s2@(ST Tripped Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2
edgeStateChecks s1@(ST Tripped Ack Max p1) s2@(ST Tripped Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2

-- Clearing States
edgeStateChecks s1@(ST Clearing Calling More p1) s2@(ST Clearing Answered More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing Calling More p1) s2@(ST Clearing Answered Max p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing Calling More p1) s2@(ST Clearing NoAnswer More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing Calling More p1) s2@(ST Clearing NoAnswer Max p2)
  |p1 == p2 = True


-----

edgeStateChecks s1@(ST Clearing Answered More p1) s2@(ST Clearing Ack More p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Clearing Answered More p1) s2@(ST Clearing NotAck More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing Answered Max p1) s2@(ST Clearing Ack Max p2)
  |p1 == p2 = True              
edgeStateChecks s1@(ST Clearing Answered Max p1) s2@(ST Clearing NotAck Max p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing NoAnswer More p1) s2@(ST Clearing Calling More p2)
  |p1 == p2 = True
edgeStateChecks s1@(ST Clearing NoAnswer Max p1) s2@(ST Clearing Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2
edgeStateChecks s1@(ST Clearing NotAck More p1) s2@(ST Clearing Calling More p2)
  |p1 == p2 = True                            
edgeStateChecks s1@(ST Clearing NotAck Max p1) s2@(ST Clearing Calling More p2)
  |p1 == maxBound && p2 == minBound = True
  |p1 /= maxBound = succ p1 == p2
edgeStateChecks s1@(ST Clearing Ack More p1) s2@(ST Clear NotCalling More p2) = True                    
edgeStateChecks s1@(ST Clearing Ack Max p1) s2@(ST Clear NotCalling More p2) = True
edgeStateChecks _ _ = False


onlyPossibleEdges :: [AlarmEdge]
onlyPossibleEdges = do 
  n1@(i1,s1) <- onlyPossibleNodes
  n2@(i2,s2) <- onlyPossibleNodes
  guard (i1 /= i2)
  guard $ edgeStateChecks s1 s2
  return (i1,i2,"")
  



\end{code}
                                                                 

\begin{code}



showGraphState (ST a c cnt p) = show a  ++ "," ++ show c ++ "," ++ show cnt ++ "," ++ show p
printGraphState (ST a c cnt p) = putStrLn $ show a  ++ " " ++ show c ++ " " ++ show cnt ++ " " ++ show p

walkThrough :: (Enum a) => Int ->  a -> [a]
walkThrough n = (take n).( iterate succ)

allPeople :: [People]
allPeople = walkThrough 3 minBound

allAlarms :: [Alarm]
allAlarms = walkThrough 4 Clear

allCalls :: [Call]
allCalls = walkThrough 6 NotCalling

allCounts :: [Count]
allCounts = walkThrough 2 More



allStates :: [GraphState]
allStates = do 
  p <- allPeople
             
  a <- allAlarms
             
  c <- allCalls 
       
  cnt <- allCounts
  return $ ST  a c cnt p





allEdgeTuples :: [(GraphState,GraphState)] 
allEdgeTuples = do 
  s1 <- onlyPossibleStates
  s2 <- onlyPossibleStates
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


trippingNodes = fromList $ [n | (n,l) <-onlyPossibleNodes , (alarm l == Tripping)]

trippedNodes  = fromList $ [n | (n,l) <-onlyPossibleNodes , (alarm l == Tripped)]

clearingNodes = fromList $ [n | (n,l) <-onlyPossibleNodes , (alarm l == Clearing)]

clearNodes = fromList $ [n | (n,l) <-onlyPossibleNodes , (alarm l == Clear)]

data EdgeType = ClearToTripping | ClearingToTripped | TrippingToTripped| TrippedToClearing | ClearingToClear |TrippingToClear |Error
              | TrippedToTripped | ClearingToClearing
  
clearToTrip n1 n2
  | (member n1 clearingNodes) && (member n2 trippedNodes) = ClearingToTripped
  | (member n1 clearingNodes) && (member n2 clearNodes)   = ClearingToClear
  | (member n1 trippingNodes) && (member n2 trippedNodes) = TrippingToTripped
  | (member n1 trippingNodes) && (member n2 clearNodes)   = TrippingToClear
  | (member n1 trippedNodes)  && (member n2 clearingNodes) = TrippedToClearing
  | (member n1 clearNodes)    && (member n2 trippingNodes) = ClearToTripping
  | (member n1 trippedNodes)  && (member n2 trippedNodes) = TrippedToTripped
  | (member n1 clearingNodes) && (member n2 clearingNodes) = ClearingToClearing
  | otherwise = Error


stateGraph :: Gr (GraphState) String
stateGraph = mkGraph onlyPossibleNodes  onlyPossibleEdges

writeGraph = writeFile "./diagrams/graph.gv" $ graphviz stateGraph "fgl" (11.0,11.0) (1,1) Portrait

goodDefaults = defaultParams {
                 globalAttributes =  [globalAttrs],
                 fmtNode = (\(n,l) -> labelMatcher l ),
                 fmtEdge = (\(n1,n2,el) -> labelEdge n1 n2 ),
                 clusterID = (\x -> Data.GraphViz.Str x)}

labelEdge n1 n2 = case clearToTrip n1 n2 of
      ClearingToTripped   ->  [Color penColorRed, PenWidth penWidth, ArrowSize arrowSize]
      ClearingToClear     ->  [Color penColorGreen, PenWidth penWidth, ArrowSize arrowSize]
      TrippingToTripped   ->  [Color penColorRed, PenWidth penWidth, ArrowSize arrowSize]
      TrippingToClear     ->  [Color penColorGreen, PenWidth penWidth, ArrowSize arrowSize]
      TrippedToClearing   ->  [Color penColorLightG, PenWidth penWidth, ArrowSize arrowSize]
      ClearToTripping     ->  [Color penColorOrange, PenWidth penWidth, ArrowSize arrowSize]
      TrippedToTripped    ->  [Color penColorRed, PenWidth penWidth, ArrowSize arrowSize]
      ClearingToClearing  ->  [Color penColorLightG, PenWidth penWidth, ArrowSize arrowSize]
      Error               ->  [Color penColorBlack, PenWidth penWidth, ArrowSize arrowSize]
  where penColorGreen  = [WC (RGB 74 212 125) Nothing]
        penColorRed    = [WC (RGB 217 4 54) Nothing]
        penColorLightG = [WC (RGB 48 8 158) Nothing]
        penColorOrange = [WC (RGB 217 114 4) Nothing]
        penColorBlack = [WC (RGB 0 0 0) Nothing]
        penWidth = 10
        arrowSize = 2



labelMatcher s@(ST a c cnt p) 
    |a == Clear = alarmClearLabel s
    | a == Clearing = alarmClearingLabel s
    |a == Tripped = case c of
      Calling -> alarmTrippedCallingLabel s
      _ -> alarmTrippedLabel s
    |a == Tripping = alarmTrippingLabel s
    | otherwise = defaultLabel s

nodeFontSize = FontSize 15.0
-- nodeWidth = Width 3.0
-- nodeHeight = Height 3.0

alarmClearLabel s = [Label $ StrLabel (T.pack.show $ s) , nodeFontSize
                     , FillColor clearColor, Shape Circle, clearStyle ]
       where clearColor = [WC  (RGB 74 212 125 ) Nothing]
             clearStyle = Style [SItem Filled []]

alarmClearingLabel s = [Label $ StrLabel (T.pack.show $ s) , nodeFontSize
                     , FillColor clearColor, Shape BoxShape, clearStyle]
       where clearColor = [WC  (RGB 225 250 197 ) Nothing]
             clearStyle = Style [SItem Filled []]


alarmTrippedLabel s = [Label $ StrLabel (T.pack.show $ s) , nodeFontSize
                      , FillColor clearColor, Shape BoxShape, clearStyle]
       where clearColor = [WC  (RGB 217 4 54 ) Nothing]
             clearStyle = Style [SItem Filled []]


alarmTrippedCallingLabel s = [Label $ StrLabel (T.pack.show $ s) , FontSize 10.0
                      , FillColor clearColor, Shape Circle, clearStyle]
       where clearColor = [WC  (RGB 217 4 54 ) Nothing]
             clearStyle = Style [SItem Filled []]


alarmTrippingLabel s = [Label $ StrLabel (T.pack.show.alarm $ s) , nodeFontSize
                       , FillColor clearColor, Shape MDiamond, clearStyle]
       where clearColor = [WC  (RGB 217 114 4 ) Nothing]
             clearStyle = Style [SItem Filled []]


defaultLabel s =  [Label $ StrLabel (T.pack.show $ s)]

globalAttrs = GraphAttrs [Size (GSize 51.0 (Just 51.0) True),
                          Scale $ PVal (createPoint 1 1),
                          RankDir FromLeft,
                          Ratio FillRatio,
                          Overlap ScaleXYOverlaps,
--                          Ratio $ AspectRatio 0.50,
                          Splines SplineEdges, 
                          FontName "courier" ]

writeGraphViz = do
                let gtd = graphToDot (goodDefaults) stateGraph
                    str = renderDot.toDot $ gtd
                TIO.writeFile "./diagrams/autograph.gv" $  str
                   
\end{code}
