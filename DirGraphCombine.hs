
{-|
Module : DirGraphCombine
Description : Evaluation of DirGraph's to FGL-Graphs, so that they can evaluated by the module graphviz.
Copyright : (c) Hans-Jürgen Guth, 2014
License : All rights reserved
Maintainer : juergen.software@freea2a.de
Stability : experimental, no known bugs
Portability : all

The Functions of this module enables you to generate FGL-Graphs for
the haskell module graphviz.
-}

module DirGraphCombine (mkVizGraph, mkGraphBeginEnd, VizGraph) where

import TestExplode3 -- (DirGraph(SimpleDG, Conc, Join, StructDG)
                    -- , SplittedGraph(Split)
                    -- , Testgraph(Testgraph)
                    -- , TGDocuInfo(TGDocuInfo)
                    -- )

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Control.Monad.State



-- | Graph for Graphviz: a FGL-Graph
-- The 'Int' is the cluster number of the Node
type VizGraph a = Gr (Maybe a, Maybe TGDocuInfo) ()  -- Gr ist definiert in PatriciaTree


-- | The heart of this module: DirGraph to VizGraph.
-- Runs the state monad 'mkGraphBeginEnd'.
mkVizGraph :: DirGraph a -> VizGraph a
mkVizGraph dirGraph = fst . fst $ runState (mkGraphBeginEnd dirGraph) empty                           


-- | the evaluate-function of the EDSL to generate a 
-- VizGraph. Normally the function 'mkVizGraph' should be sufficent for the end-user.
-- The state is a 'VizGraph', that is used to apply the function 
-- 'newNodes' to it, that gives one or more new nodes.
-- In this VizGraph the Nodes are simply added, edges are senseless and
-- not added.
-- The output is the resulting VizGraph and a tuple of the first Node
-- and the last Node (remember: a DirGraph has exactly one begin and one end).
-- Aditionally new in this version (28.12.2014) the state contains 
-- a cluster-Number. Every new subgraph ('StructDG') gets a induvidual
-- cluster number, that is used to print the graph in clusters. 
mkGraphBeginEnd :: DirGraph a ->  State (VizGraph a) (VizGraph a, (Node, Node))
mkGraphBeginEnd (SimpleDG x) =
  do
    stateGraph <- get
    let newNode1 = head $ newNodes 1 stateGraph
    put $ insNode (newNode1, (Just x, Nothing)) stateGraph
    let returnGraph  = insNode (newNode1, (Just x, Nothing)) empty 
    return (returnGraph, (newNode1, newNode1))
  --  alternative:      
  --                    state (
  --                        \oldGraph ->
  --                         let 
  --                           newNode1 = head $ newNodes 1 oldGraph
  --                           newGraph = insNode (newNode1, Just x) oldGraph
  --                           returnGraph  = insNode (newNode1, Just x) empty  
  --                         in
  --                           ((returnGraph, (newNode1, newNode1)), newGraph )                      
  --                         )  

mkGraphBeginEnd (Conc dirGraph1 dirGraph2) = 
   do 
     (graph1, (nid11,nid12)) <- mkGraphBeginEnd dirGraph1
     (graph2, (nid21,nid22)) <- mkGraphBeginEnd dirGraph2
     let bigGraph1 = addGraph graph1 graph2
     -- Verbindungskante einfuegen
     --   (hier koennte noch eine Reduzierung um 
     --    "Nothing"-Nodes hin, die nur erzeugt wurden,
     --    damit es 1 Anfang und 1 Ende eines gesplitteten
     --    Graphen gibt)
     let bigGraph2 = insEdge (nid12, nid21, ()) bigGraph1
     -- Ergebnis zuweisen:
     -- 2. alles zusammen
     return (bigGraph2, (nid11, nid22))
                      
mkGraphBeginEnd (Join splittedGraph) = 
   do
     -- alle Graphen auswerten
     newGraphs <- mkSplittedGraph (splittedGraph)
     -- den status, d.h. die Menge aller Knoten 
     -- und aktuelle Cluster-Nummer holen
     currState1 <- get
     -- aufbauend auf dem Status 2 neue Knoten generieren
     let newNode1:newNode2:[] = newNodes 2 currState1
     -- nodes in den Status einfuegen,
     -- mehr muss mit dem Status nicht gemacht werden
     put $ (insNodes [(newNode1, (Nothing, Nothing)), (newNode2, (Nothing, Nothing))] currState1)
     -- nodes in den return-Graphen einfuegen
     let newGraph1 = insNodes [(newNode1, (Nothing, Nothing)), (newNode2, (Nothing, Nothing))] empty
     -- alle neuen Graphen in den return-Graphen einfuegen
     let newGraph2 = foldr (addGraph . fst) newGraph1 newGraphs
     -- neue Kanten erstellen
     let newBeginEdges = mkBeginNode newNode1 newGraphs
     let newEndEdges = mkEndNode newNode2 newGraphs
     -- neue Kanten einfuegen
     let newGraph3 = insEdges (newBeginEdges ++ newEndEdges) newGraph2
     -- fertig!
     return (newGraph3, (newNode1, newNode2)) 
     
mkGraphBeginEnd (StructDG tg) =
   do
     case toExpand (docuInfo tg) of
       True  -> mkGraphBeginEnd (dirGraph tg)
       False -> do 
                  stateGraph <- get
                  let newNode1 = head $ newNodes 1 stateGraph
                  put $ insNode (newNode1, (Nothing, Just (docuInfo tg))) stateGraph
                  let returnGraph  = insNode (newNode1, (Nothing, Just (docuInfo tg))) empty 
                  return (returnGraph, (newNode1, newNode1)) 

-- Ein SplittedGraph wird ausgewertet und als Liste zurueckgegeben                      
mkSplittedGraph :: SplittedGraph a ->  State (VizGraph a) [(VizGraph a, (Node, Node))]
mkSplittedGraph (Split [])      = return []
mkSplittedGraph (Split (x:xs))  =
   do
     newErg   <- mkGraphBeginEnd x 
     newList  <- mkSplittedGraph (Split xs)
     return (newErg : newList)
                       
                       
-- Hilfsfunktion, um einen Knoten mit allen In- oder Outnodes zu verbinden
mkBeginNode :: Node -> [(VizGraph a, (Node, Node))] -> [(Node, Node, ())]
mkBeginNode node [] = []
mkBeginNode node ((_, (node1,node2)):xs) =
            (node, node1,()) : (mkBeginNode node xs)
            
-- Hilfsfunktion, um einen Knoten mit allen In- oder Outnodes zu verbinden
mkEndNode :: Node -> [(VizGraph a, (Node, Node))] -> [(Node, Node, ())]
mkEndNode node [] = []
mkEndNode node ((_, (node1,node2)):xs) =
            (node2, node,()) : (mkEndNode node xs)


-- Hilfsfunktion, um zwei Graphen zu Einen (meist disjunkten) zu machen
-- precondition: different node-id's in bigGraph and smallGraph
addGraph :: Gr a b-> Gr a b -> Gr a b
addGraph bigGraph smallGraph =
      -- doesn't work, makes double edges
      --  let smallContexts = map (context smallGraph) (nodes smallGraph)
      --      bigContexts = map (context bigGraph) (nodes bigGraph)
      --      allContexts = smallContexts ++ bigContexts
      --  in
      --      buildGr allContexts
         -- Alternative:
         -- also doesn't work, makes other double edges
         -- foldr (&) bigGraph (map (context smallGraph) (nodes smallGraph))
         let biggerGraph = insNodes (labNodes smallGraph) bigGraph
         in
         insEdges  (labEdges smallGraph) biggerGraph
         
 
