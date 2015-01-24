
{-|
Module : VizViews
Description : Defines the some different look of graphs for the module graphviz.
Copyright : (c) Hans-Jürgen Guth, 2014
License : All rights reserved
Maintainer : testexplode@freea2a.org
Stability : experimental, no known bugs
Portability : all

Here is the look of the graphs from graphvis defined.
Feel free to add your own. For more information to the source code,
refer to the docu of the module graphviz.
-}

module VizViews (recordView1, defaultView) where

import TestExplode3

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as L


-- | From the docu of GraphViz. Only numbers in the nodes.
-- But as a test it works.
defaultView :: (Graph gr) => gr nl el -> DotGraph Node
defaultView = graphToDot nonClusteredParams

-- | My own favorite view of the graph
recordView1 :: GraphvizParams Node (Maybe (Casepart cnf), Maybe TGDocuInfo) 
                                   () 
                                   () 
                                   (Maybe (Casepart cnf), Maybe TGDocuInfo)
recordView1 = nonClusteredParams { fmtNode = fmtNodeMy }

fmtNodeMy :: (Node, (Maybe (Casepart cnf), Maybe TGDocuInfo)) 
              -> Attributes
fmtNodeMy (_, mcp) = case mcp of
              (Just cp, _) -> 
                 let bgcolor = if cpType cp == Mark
                               then LightGoldenrod2 --YellowGreen
                               else
                                 if condDesc cp == ""
                                 then  LightGoldenrodYellow 
                                 else Yellow 
                 in              
                 let attrs = [  shape Record
                              , style filled]
                 in 
                  if condDesc cp ==""
                  then   [Label (RecordLabel [FlipFields[ FieldLabel (L.pack ((shortDesc cp) ))
                                                  ]
                                       ]
                           ), fillColor bgcolor] 
                          ++ attrs
                  else   [Label (RecordLabel [FlipFields[  FieldLabel (L.pack (condDesc cp))
                                                   , FieldLabel (L.pack ((shortDesc cp) ))
                                                  ]
                                       ]
                           ), fillColor bgcolor] 
                          ++ attrs
              (Nothing, Nothing) -> 
                [ styles [filled, dotted]
                  , fillColor LightGray
                  , toLabel ""]
              (Nothing, Just di) ->  -- testgraph that is not expanded
                [shape Record,
                 styles [bold, filled],
                 fillColor Orange,
                 fontColor Blue,
                 FontName (L.pack("Times-Italic")),
                 URL (L.pack("../subgraphs/" ++ name di ++ ".svg")),
                 Label (RecordLabel [FlipFields[ FieldLabel (L.pack ((name di) 
                                                             ++ "\n" 
                                                             ++ (descForNode di) ))
                                                ]
                                     ])]

