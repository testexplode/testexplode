{-# LANGUAGE OverloadedStrings #-}
{-|
Module : TestExplode.TestExplode
Description : Definitions of part-testcases, graphs of them, and an evaluation function to generate complete testcases (strings).
Copyright : (c) Hans-Jürgen Guth, 2014
License : All rights reserved
Maintainer : juergen.software@freea2a.de
Stability : experimental

With this module you can define a graph of part-testcases
('Casepart') and evaluate this graph to a list of strings,
where every string is the concatenation of the code of the
part-testcases.
-}

module TestExplode.TestExplode (
                    -- * Types
                    Casepart(Casepart)
                    , shortDesc
                    , longDesc
                    , condDesc
                    , codeFkt
                    , varFkt
                    , condition
                    , CasepartInternal(CasepartInternal)
                    , shortDescI
                    , longDescI
                    , condDescI
                    , codeFktI
                    , conditionI
                    , CPType(NormalCP, Mark)
                    , cpType
                    , cpTypeI
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
                    , generate
                    , emptyCp
                    , markCp
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
                    -- * Conversion Functions
                    , convertDirGraph
                    , convertTestgraph  
                    ) where
 
import Control.Monad.Writer 
import qualified Data.Text.Lazy as L
import qualified Data.Sequence as S
import qualified Data.Foldable as DF                  

-- | The part-testcase
data Casepart cnf     -- the test data
              locals  -- test hints, that are changed by a Casepart
                      --  for example  the state of a state machine
                      --  or the time since start of the Testcase  
              = Casepart 
                  { 
                    -- | short description of the part-testcase,
                    -- currently used  a) at top of a testcase to show
                    -- which path the generated testcase belongs to
                    -- and b) in the visualised graph as node-label  
                    shortDesc :: L.Text
                    -- | long description of the part-testcase
                    -- currently generated in front of the code of the
                    -- part-testcase
                    , longDesc :: L.Text
                    -- | description of the condition, under which
                    -- the part-testcase is valid (if not, the path with
                    -- this part-testcase will not be generated)
                    , condDesc :: L.Text
                    -- | the actual code, which forms the part-testcase,
                    -- dependent of the "configuration" (the  "cnf" in 
                    -- 'Casepart cnf locals'), which is the test-data, 
                    -- and local variables, that are changed by a Casepart.
                    , codeFkt :: cnf -> locals -> L.Text
                    -- | The changes in the local variables
                    , varFkt :: cnf -> locals -> locals
                    -- | the condition under which the part-testcase
                    -- is valid (if not, the path with
                    -- this part-testcase will not be generated)
                    , condition :: cnf -> locals -> Bool
                    -- | Type of the Casepart, mainly (up to now only) for
                    -- visualisation in the graph of Caseparts
                    , cpType :: CPType
                  }
                  
-- | The part-testcase, internal format of 'Casepart', with a writer-monad as stringFkt 
-- instead of the varFkt and the old stringFkt
data CasepartInternal cnf locals = CasepartInternal 
                  { 
                    shortDescI :: L.Text
                    , longDescI :: L.Text
                    , condDescI :: L.Text
                    , codeFktI :: cnf -> locals -> Writer (S.Seq L.Text) locals
                    , conditionI :: cnf -> locals -> Bool
                    , cpTypeI :: CPType
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
                    , codeFkt = \cnf locals -> ""
                    , varFkt = \cnf locals -> locals 
                    , condition = \cnf locals -> True
                    , cpType = NormalCP
                    } 

                   
-- | Convenience Function to make easily a mark.
--                   
markCp str = emptyCp { shortDesc = L.append "Mark: " str,
                       longDesc = "Set a mark",
                       codeFkt = \cnf locals -> L.concat["  setCheckMark(\"",
                                                         str,
                                                         "\");\n"],
                       cpType = Mark
                     }                  
-- | The heart of this module, the final function.
--  It takes configurations ('cnf' means testvalues),
--  that is a record of variables with a
-- value, a function that describes the "prelude" of one testcase (without
-- comment chars, which are later added) (a good starting value : the 
-- 'show'-function of 'cnf', so that the used test-values are printed on top
-- of the testcase), the graph of testcases and returns 
-- 
-- voilá:
--
-- the 
-- list of testcases, ready to printed out in seperate files and to run.
generate ::  L.Text     -- ^ how a text is coomented, ("# " or "-- ")
             -> [cnf]   -- ^ a list of the testvalues
             -> locals  -- ^ the initial value of the variables that the
                        --   testcases change
             -> (cnf -> L.Text) -- ^ "prelude" of a testcase, i.e. 'show' of cnf
             -> DirGraph (CasepartInternal cnf locals)  -- ^ the graph of caseparts
             -> [L.Text]  -- ^ the final result: the list of testcases incl. comments
generate commentString cnfList locals cnfShow graph = 
        [L.concat[mkComment(L.concat[(cnfShow cnf),"\n", desc]) commentString,
                  DF.fold $ snd $ runWriter (stringFkt cnf locals)
                 ] | 
                         cnf <- cnfList,
                         -- Casepart stringFkt cond <- cpGetPaths graph,
                         -- cond cnf] 
                         --
                         -- Does this work too? is independent of the 
                         --   structure, uses record syntax
                         let cpList = cpGetPaths commentString graph,
                         (stringFkt, cond, desc) <- map getCodeAndConditionAndDesc cpList, 
                         cond cnf locals]
                      
-- | Internal help function, could be made more general for
--   arbitrary getter functions and number of getter functions 
getCodeAndConditionAndDesc :: CasepartInternal a b -> 
                              ((a -> b -> Writer (S.Seq L.Text) b), (a -> b ->Bool), L.Text)
getCodeAndConditionAndDesc cp =  (codeFktI cp, conditionI cp, shortDescI cp)

-- | Internal function to comment the description with the commentString
mkComment :: L.Text -> L.Text -> L.Text
--mkComment str  = let strNew = "# " ++ str
--                in
--                  Utils.replace "\n" "\n# " str
mkComment str commentString = L.unlines $ map (L.append commentString ) (L.lines str)

 
-- Now all the functions for combinating Casepart's

-- | Directed graph with one end, self-invented definition
data DirGraph a =        -- | Constructor for a node alone, 
                         --  A node is a graph. 
                         SimpleDG a | 
                         -- | Constructor for one sub-graph after another
                         Conc (DirGraph a) (DirGraph a) | 
                         -- | Constructor for the "splitting" of graphs,
                         -- comparable with an "if".
                         -- The 'Join' makes the many ends and begins 
                         -- to one end and one begin
                         Join (SplittedGraph a) |
                         -- | A graph with more attributes,
                         --   importing of a 'Testgraph', only the part
                         --   'dirGraph' is used
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
split :: [DirGraph a] -> DirGraph a
split x = Join (Split x)

-- | Function to craete a 'Conc' of two sub-graphs
conc :: DirGraph a -> DirGraph a -> DirGraph a
conc a b = Conc a b

-- | Infix synonym for 'conc'
(&-&) :: DirGraph a -> DirGraph a -> DirGraph a
a &-& b = Conc a b

-- | Function to create a node, Internal, with the CasepartInternal as 'a'
mkEleInt :: a -> DirGraph a
mkEleInt a = SimpleDG a

-- | Function to create a node, function for the user.
--   If longDesc = "", shortDesc is used as longDesc.
mkEle :: Casepart cnf locals -> DirGraph (CasepartInternal cnf locals)
mkEle cpUser = mkEleInt (CasepartInternal {shortDescI = shortDesc cpUser,
                                   longDescI = if longDesc cpUser == "" 
                                               then shortDesc cpUser
                                               else longDesc cpUser,
                                   condDescI = condDesc cpUser,
                                   codeFktI = mkLogging  (codeFkt cpUser) (varFkt cpUser),
                                   conditionI = condition cpUser,
                                   cpTypeI    = cpType cpUser
                                  })
                                  
                                  
-- | Internal Function to build the monad-function as the new codeFkt
mkLogging :: (cnf -> locals -> L.Text)        -- ^ the old codeFkt
              -> (cnf -> locals -> locals)    -- ^ the change-function of the variables  (old varFkt)
              ->  (cnf -> locals -> Writer (S.Seq L.Text) locals)  -- ^ the new codeFkt
mkLogging fText fVars =  \cnf locs ->  
                           let ret = fVars cnf locs
                           in
                           do tell $ S.singleton $ fText cnf locs
                              return ret



data Expand = Expand | NotExpand | AsIs

-- | Function to add a testgraph to a dirgraph
-- with converting-function f of the testdata ("cnfOld" resp. "cnfNew")
-- and a Boolean, that says, if the subgraph should be 
-- expanded or not.
mkGraph2Ele :: (cnfNew -> cnfOld) -- ^ conversion function for the test-data-input of the casepart
               -> (localsInB -> localsInA) -- ^ conversion function for the 
                                           --   variables the testcases uses/changes (input-side)
               -> (localsInB -> localsInA -> localsInB) -- ^ conversion function for the 
                                               --   variables the testcases uses/changes (output-side)
                                               -- that is: how shall the variables look after the run 
                                               --  of the casepart? Dependant of the old value
                                               -- of the variables and the value of the variables after run
                                               -- of the imported testcase
               -> Expand  -- ^ Shall this Graph in the documation expanded or not ?
               -> Testgraph (CasepartInternal cnfOld localsInA) -- ^ the Testgraph that shall be imported
               -> DirGraph (CasepartInternal cnfNew localsInB) -- ^ the imported Testgraph, now a DirGraph
                                                               -- with the correct types
mkGraph2Ele fCnf fLocIn fLocOut expand tg = 
               let newTg = case expand of
                     AsIs      -> tg
                     NotExpand -> tg {docuInfo=(docuInfo tg) {toExpand=False}}
                     Expand    -> tg {docuInfo=(docuInfo tg) {toExpand=True}}
               in              
               StructDG ( convertTestgraph fCnf fLocIn fLocOut newTg)
               
               
-- | Function to add a testgraph to a dirgraph
-- without converting-function
mkGraph2Ele0 :: Testgraph a 
                -> DirGraph a
mkGraph2Ele0 tg = StructDG tg


-- | The eval function of the EDSL. Evaluates a 'DirGraph' to the list 
-- of all paths.
cpGetPaths ::  L.Text -> DirGraph (CasepartInternal cnf locals ) -> [CasepartInternal cnf locals]
cpGetPaths commentString (SimpleDG cp) = 
          let lngDesc = longDescI cp
              cdFkt = codeFktI cp
          in 
          -- insert longDesc before codeFkt
          [cp{codeFktI = \cfg locals -> do 
                                          tell $ S.singleton "\n"
                                          tell $ S.singleton $ mkComment lngDesc commentString
                                          cdFkt cfg locals
             }] 
   
cpGetPaths commentString (Conc dirGraph1 dirGraph2) =
   let paths1 = cpGetPaths commentString dirGraph1
       paths2 = cpGetPaths commentString dirGraph2
       in 
         [CasepartInternal { 
                    longDescI="" -- not relevant for combined part-testcases
                    ,condDescI="" -- not relevant for combined part-testcases
                    ,cpTypeI = NormalCP -- not relevant for combined part-testcases
                    ,shortDescI = L.concat[shortDescI cp1,
                                           "\n and\n",
                                          shortDescI cp2]
                    ,codeFktI = \cnf locals ->  do 
                                                vars1 <- (codeFktI cp1) cnf locals
                                                (codeFktI cp2) cnf vars1
                    ,conditionI = \cnf locals -> (((conditionI cp1) cnf locals) && ((conditionI cp2) cnf locals))} |
                       cp1 <- paths1,
                       cp2 <- paths2 ] -- jeder mit jedem
                       
cpGetPaths commentString (StructDG tg) = cpGetPaths commentString (dirGraph tg)
                                                      
cpGetPaths commentString (Join (Split paths )) =  concat $ lcpGetPaths commentString (Split paths)

-- | the eval function of the EDSL for SplittedGraphs   
lcpGetPaths :: L.Text -> SplittedGraph (CasepartInternal cnf locals) -> [[CasepartInternal cnf locals]]
lcpGetPaths commentString (Split paths) = map (cpGetPaths commentString) paths


-- | Converts between Caseparts.
-- You need a interpreting from the target data-type to the
-- source data-type (not vice versa) 
convertCasepart :: (cnfB -> cnfA)  -- ^ conversion function for the test-data-input of the casepart
                   -> (localsInB -> localsInA) -- ^ conversion function for the 
                                               --   variables the testcases uses/changes (input-side)
                   -> (localsInB -> localsInA -> localsInB) -- ^ conversion function for the 
                                               --   variables the testcases uses/changes (output-side)
                                               -- that is: how shall the variables look after the run 
                                               --  of the casepart? Dependant of the old value
                                               -- of the variables and the value of the variables after run
                                               -- of the imported testcase
                   -> CasepartInternal cnfA localsInA -- ^ the Casepart that shall be imported
                   -> CasepartInternal cnfB localsInB -- ^ the imported Casepart with the correct types
convertCasepart fCnf fLocIn fLocOut cpa = 
                CasepartInternal { 
                                   codeFktI = \cnf locals -> do
                                                               oldIn <- (codeFktI cpa) (fCnf cnf) (fLocIn locals)
                                                               return $ fLocOut locals oldIn 
                                                             ,
                                   conditionI = \cnf locals -> (conditionI cpa) (fCnf cnf) (fLocIn locals),
                                   shortDescI = shortDescI cpa,
                                   longDescI = longDescI cpa,
                                   condDescI = condDescI cpa,
                                   cpTypeI = cpTypeI cpa                                  
                                 }
                                 
-- | Converts a DirGraph, for example our testgraphs.
-- With that function you can import other testgraphs
-- with another set of variables.  
-- You need a interpreting from the target data-type to the
-- source data-type (not vice versa)                              
convertDirGraph :: (cnfB->cnfA) -- ^ conversion function for the test-data-input of the casepart
                   -> (localsInB -> localsInA)  -- ^ conversion function for the 
                                                --   variables the testcases uses/changes (input-side)
                   -> (localsInB -> localsInA -> localsInB) -- ^ conversion function for the 
                                               --   variables the testcases uses/changes (output-side)
                                               -- that is: how shall the variables look after the run 
                                               --  of the casepart? Dependant of the old value
                                               -- of the variables and the value of the variables after run
                                               -- of the imported testcase
                   -> DirGraph (CasepartInternal cnfA localsInA) -- ^ the DirGraph that shall be imported
                   -> DirGraph (CasepartInternal cnfB localsInB) -- ^ the imported DirGraph with the correct types
convertDirGraph f fLocIn fLocOut (SimpleDG cp) = SimpleDG (convertCasepart f fLocIn fLocOut cp)
convertDirGraph f fLocIn fLocOut (Conc dg1 dg2)= Conc (convertDirGraph f fLocIn fLocOut dg1)
                                       (convertDirGraph f fLocIn fLocOut dg2)
                                    
convertDirGraph f fLocIn fLocOut (Join splittedGraph) = 
       Join ( convertSplittedGraph f fLocIn fLocOut splittedGraph)
       


-- | Converts a SplittedGraph
convertSplittedGraph :: (cnfB->cnfA)
                        -> (localsInB -> localsInA)
                        -> (localsInB -> localsInA -> localsInB) 
                        -> SplittedGraph (CasepartInternal cnfA localsInA) 
                        -> SplittedGraph (CasepartInternal cnfB localsInB)
convertSplittedGraph f fLocIn fLocOut (Split dirGraphs) =
       Split (map (convertDirGraph f fLocIn fLocOut) dirGraphs)  
       
       
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
convertTestgraph :: (cnfB -> cnfA)   -- ^ conversion function for the test-data-input of the casepart
                    -> (localsInB -> localsInA) -- ^ conversion function for the 
                                                --   variables the testcases uses/changes (input-side)
                    -> (localsInB -> localsInA -> localsInB) -- ^ conversion function for the 
                                               --   variables the testcases uses/changes (output-side)
                                               -- that is: how shall the variables look after the run 
                                               --  of the casepart? Dependant of the old value
                                               -- of the variables and the value of the variables after run
                                               -- of the imported testcase
                    -> Testgraph (CasepartInternal cnfA localsInA) -- ^ the Testgraph that shall be imported
                    -> Testgraph (CasepartInternal cnfB localsInB) -- ^ the imported Testgraph with the correct types            
convertTestgraph f fLocIn fLocOut tg = tg { dirGraph = convertDirGraph f fLocIn fLocOut (dirGraph tg)} 

-- | Convenience function for the case, that the return value of an 
--   embedded 'Casepart' shall have no effect. The old local
--   values keep unchanged.
emptyOut :: localsInA -> localsInB -> localsInA
emptyOut fromMaster fromEmbedded = fromMaster  



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


