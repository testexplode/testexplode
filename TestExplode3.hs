
{-|
Module : TestExplode3
Description : Definitions of part-testcases, graphs of them, and an evaluation function to generate complete testcases (strings).
Copyright : (c) Hans-Jürgen Guth, 2014
License : All rights reserved
Maintainer : testexplode@freea2a.org
Stability : experimental, no known bugs
Portability : all

With this module you can define a graph of part-testcases
('Casepart') and evaluate this graph to a list of strings,
where every string is the concatenation of the code of the
part-testcases.
-}

module TestExplode3 (
                    -- * Types
                    Casepart(Casepart)
                    , shortDesc
                    , longDesc
                    , condDesc
                    , codeFkt
                    , condition
                    , CPType(NormalCP, Mark)
                    , cpType
                    , DirGraph(SimpleDG, Conc, Join, StructDG)
                    , SplittedGraph(Split)
                    , Testgraph(Testgraph)
                    , dirGraph
                    , docuInfo
                    , TGDocuInfo(TGDocuInfo)
                    , name
                    , descForNode
                    , descForTex
                    , generic
                    , toExpand
                    , Expand (Expand, NotExpand, AsIs)                  
                    -- * Functions
                    , emptyCp
                    , markCp
                    , generate
                    , convertDirGraph
                    , convertTestgraph
                    , getSubGraphs
                    -- * Functions for generating 'DirGraph' s 
                    -- The non-graphical-UI for the user.
                    -- Call it a EDSL, if you like
                    , mkEle
                    , (&-&)
                    , conc
                    , split
                    , mkGraph2Ele
                    , mkGraph2Ele0
                    ) where
                   

-- | The part-testcase
data Casepart a = Casepart 
                  { 
                    -- | short description of the part-testcase,
                    -- currently used  a) at top of a testcase to show
                    -- which path the generated testcase belongs to
                    -- and b) in the visualised graph as node-label  
                    shortDesc :: String
                    -- | long description of the part-testcase
                    -- currently generated in front of the code of the
                    -- part-testcase
                    , longDesc :: String
                    -- | description of the condition, under which
                    -- the part-testcase is valid (if not, the path with
                    -- this part-testcase will not be generated)
                    , condDesc :: String
                    -- | the actual code, which forms the part-testcase,
                    -- dependent of the "configuration" (the  "a" in 
                    -- 'Casepart a')
                    , codeFkt :: (a -> String)
                    -- | the condition under which the part-testcase
                    -- is valid (if not, the path with
                    -- this part-testcase will not be generated)
                    , condition :: (a -> Bool)
                    -- | Type of the Casepart, mainly (up to now only) for
                    -- visualisation in the graph of Caseparts
                    , cpType :: CPType
                  }
                  
-- | Types of Caseparts, mainly (up to now only) for
-- visualisation of the graph of Caseparts
data CPType = NormalCP | Mark
  deriving (Show, Eq)
                  
-- | An empty testcase, all strings are "".
--  The condition is always 'True'.
--  Shall serve as an starting point for own definitions of
-- 'Casepart''s.
emptyCp = Casepart {  shortDesc = ""
                    , longDesc = ""
                    , condDesc =""
                    , codeFkt = const ""
                    , condition = const True
                    , cpType = NormalCP
                   } 
                   
-- | Convenience Function to make easily a mark.
--                   
markCp str = emptyCp { shortDesc = "Mark: " ++ str,
                       longDesc = "Set a mark",
                       codeFkt = const $ "  setCheckMark(\"" ++ str ++"\");\n",
                       cpType = Mark
                     }                  
-- | The heart of this module, the final function.
--  It takes configurations ('cnf'), that is a record of variables with a
-- value, a function that describes the "prelude" of one testcase (without
-- comment chars, which are later added) (a good starting value : the 
-- 'show'-function of 'cnf', so that the used values are printed on top
-- of the testcase), the graph of testcases and returns 
-- 
-- voilá:
--
-- the 
-- list of testcases, ready to printed out in seperate files and to run.
generate ::  [cnf] -> (cnf -> String) -> DirGraph (Casepart cnf) -> [String]
generate cnfList cnfShow graph = 
        [mkComment((cnfShow cnf) ++  "\n" ++  desc) ++ (stringFkt cnf) | 
                         cnf <- cnfList,
                         -- Casepart stringFkt cond <- cpGetPaths graph,
                         -- cond cnf] 
                         --
                         -- Does this work too? is independent of the 
                         --   structure, uses record syntax
                         let cpList = cpGetPaths graph,
                         (stringFkt, cond, desc) <- map getCodeAndConditionAndDesc cpList, 
                         cond cnf]
                      
-- help function, could be made more general for
--   arbitrary getter functions and number of getter functions 
getCodeAndConditionAndDesc :: Casepart a -> ((a->String), (a->Bool), String)
getCodeAndConditionAndDesc cp =  (codeFkt cp, condition cp, shortDesc cp)

-- quick function to comment the description in ruby-style
mkComment :: String -> String
--mkComment str = let strNew = "# " ++ str
--                in
--                  Utils.replace "\n" "\n# " str
mkComment str = unlines $ map ("# " ++) (lines str)

 
-- Now all the functions for combinating Casepart's

-- | Directed graph with one end, self-invented definition
data DirGraph a =        -- | Constructor for a node alone, 
                         --  A node is a graph. 
                         SimpleDG a | 
                         -- | Constructor for one sub-graph after another
                         Conc (DirGraph a) (DirGraph a) | 
                         -- | Constructor for the "splitting" of graphs,
                         -- comparable with an "if".
                         -- The 'Join' makes the many ends end begins 
                         -- to one end and one begin
                         Join (SplittedGraph a) |
                         -- | A big graph with more attributes
                         StructDG (Testgraph a)

-- | many disjunct graphs 
-- Every part-graph has one end and one begin
data SplittedGraph a = Split [DirGraph a]

-- shorthand:
-- type CPDirGraph = DirGraph (Casepart cnf)

-- to build a directed graph:
-- at best:
-- ele1 conc ele2 conc (split [ele3, ele4, split [ele5, ele6], ele7]) conc ele8
--
-- this leads to::
-- eleN is a SimpleDG
-- conc can be infix: `conc`
-- the end of an split is a `join`

-- | Function to create a splitted graph 
split :: [DirGraph (Casepart cnf)] -> DirGraph (Casepart cnf)
split x = Join (Split x)

-- | Function to craete a 'Conc' of two sub-graphs
conc :: DirGraph (Casepart cnf) -> DirGraph (Casepart cnf) -> DirGraph (Casepart cnf)
conc a b = Conc a b

-- | Infix synonym for 'conc'
(&-&) :: DirGraph (Casepart cnf) -> DirGraph (Casepart cnf) -> DirGraph (Casepart cnf)
a &-& b = Conc a b

-- | Function to create a node
mkEle :: Casepart cnf -> DirGraph (Casepart cnf)
mkEle a = SimpleDG a


data Expand = Expand | NotExpand | AsIs

-- | Function to add a testgraph to a dirgraph
-- with converting-function f
-- and a Boolean, that says, if the subgraph should be 
-- expanded or not.
mkGraph2Ele :: (cnfNew -> cnfOld)
               -> Expand  
               -> Testgraph (Casepart cnfOld)
               -> DirGraph (Casepart cnfNew) 
mkGraph2Ele f expand tg = 
               let newTg = case expand of
                     AsIs      -> tg
                     NotExpand -> tg {docuInfo=(docuInfo tg) {toExpand=False}}
                     Expand    -> tg {docuInfo=(docuInfo tg) {toExpand=True}}
               in              
               StructDG ( convertTestgraph f newTg)
               
               
-- | Function to add a testgraph to a dirgraph
-- without converting-function
mkGraph2Ele0 :: Testgraph (Casepart cnf) 
                -> DirGraph (Casepart cnf)
mkGraph2Ele0 tg = StructDG tg


-- | The eval function of the EDSL. Evaluates a 'DirGraph' to the list 
-- of all paths.
cpGetPaths ::  DirGraph (Casepart cnf) -> [Casepart cnf]
cpGetPaths (SimpleDG cp) = let lngDesc = longDesc cp
                               cdFkt = codeFkt cp
                           in 
          -- here insert longDesc before codeFkt
          [cp{codeFkt = \cfg -> "\n" ++ (mkComment lngDesc ) ++ (cdFkt cfg)}] 
   
cpGetPaths (Conc dirGraph1 dirGraph2) =
   let paths1 = cpGetPaths dirGraph1
       paths2 = cpGetPaths dirGraph2
       in 
         [Casepart { longDesc="" -- not relevant for combined part-testcases
                    ,condDesc="" -- not relevant for combined part-testcases
                    ,cpType = NormalCP -- not relevant for combined part-testcases
                    ,shortDesc = (shortDesc cp1) ++ "\n and\n" ++ (shortDesc cp2)
                    ,codeFkt = \cnf -> ((codeFkt cp1 cnf) ++ (codeFkt cp2 cnf))
                    ,condition = \cnf -> ((condition cp1 cnf) && (condition cp2 cnf))} |
                       cp1 <- paths1,
                       cp2 <- paths2 ] -- jeder mit jedem
                       
cpGetPaths (StructDG tg) = cpGetPaths (dirGraph tg)
                                                      
cpGetPaths (Join (Split paths )) =  concat $ lcpGetPaths (Split paths)

-- | the eval function of the EDSL for SplittedGraphs   
lcpGetPaths :: SplittedGraph (Casepart cnf) -> [[Casepart cnf ]]
lcpGetPaths (Split paths) = map cpGetPaths paths


-- | Converts between Caseparts.
-- You need a interpreting from the target data-type to the
-- source data-type (not vice versa) 
convertCasepart :: (b -> a) -> Casepart a -> Casepart b
convertCasepart f cpa = Casepart { codeFkt = (codeFkt cpa) . f,
                                   condition = (condition cpa) . f,
                                   shortDesc = shortDesc cpa,
                                   longDesc = longDesc cpa,
                                   condDesc = condDesc cpa,
                                   cpType = cpType cpa                                  
                                 }
-- | Converts a DirGraph, for example our testgraphs.
-- With that function you can import other testgraphs
-- with another set of variables.  
-- You need a interpreting from the target data-type to the
-- source data-type (not vice versa)                              
convertDirGraph :: (b->a) -> DirGraph (Casepart a) -> DirGraph (Casepart b)
convertDirGraph f (SimpleDG cp) = SimpleDG (convertCasepart f cp)
convertDirGraph f (Conc dg1 dg2)= Conc (convertDirGraph f dg1)
                                       (convertDirGraph f dg2)
                                    
convertDirGraph f (Join splittedGraph) = 
       Join ( convertSplittedGraph f splittedGraph)
       


-- | Converts a SplittedGraph
convertSplittedGraph :: (b->a) -> SplittedGraph (Casepart a) -> SplittedGraph (Casepart b)
convertSplittedGraph f (Split dirGraphs) =
       Split (map (convertDirGraph f) dirGraphs)  
       
       
-- Extensions to the modules
-- for adding parts of testgraph / subgraphs

data TGDocuInfo = 
     TGDocuInfo { name        :: String,
                  descForNode :: String,
                  descForTex  :: String,
                  generic     :: Bool,
                  toExpand    :: Bool
                }           

data Testgraph a = 
     Testgraph { dirGraph  :: DirGraph a,
                 docuInfo  :: TGDocuInfo
               }

-- | Converts a testgraph, necessary in order to add 
--  a different testgraph ( with another type of configuration)
--  to a dirGraph
convertTestgraph :: (b -> a) -> Testgraph (Casepart a) -> Testgraph (Casepart b)             
convertTestgraph f tg = tg { dirGraph = convertDirGraph f (dirGraph tg)} 

-- | Looks for all embedded 'Testgraph' in a 'DirGraph',
-- double embedded 'Testgraph' (identified by the attribute 'name')
-- are ignored.
getSubGraphs :: DirGraph a -> [(String, Testgraph a)] -> [(String, Testgraph a)]
getSubGraphs (SimpleDG cp) resList = resList
getSubGraphs (Conc dirGraph1 dirGraph2) resList =
             let newResList1 = getSubGraphs dirGraph1 resList
                 newResList2 = getSubGraphs dirGraph2 newResList1 
             in 
             newResList2
getSubGraphs (Join (Split dirGraphs)) resList = foldr getSubGraphs resList dirGraphs
getSubGraphs (StructDG tg) resList= case (lookup (name (docuInfo tg)) resList) of
                             Nothing -> let newResList = (name (docuInfo tg), tg):resList
                                        in 
                                        getSubGraphs(dirGraph tg) newResList
                             Just _  -> resList


