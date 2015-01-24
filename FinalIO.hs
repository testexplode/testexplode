
{-|
Module : FinalIO
Description : The Final Functions for the end user. Writes many files.
Copyright : (c) Hans-Jürgen Guth, 2014
License : All rights reserved
Maintainer : testexplode@freea2a.org
Stability : experimental, no known bugs
Portability : all

This Functions generate the testcases as executables and as visualisation.
-}

module FinalIO (printTestcases, printTestgraph, printTestgraphP) where

import DirGraphCombine (VizGraph)
import TestExplode3 (TGDocuInfo)

import Data.List

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz

import qualified Data.Text.Lazy as L

import System.IO
import System.Process
import System.Environment
import System.Directory


induvidualizeTestcase :: String -> (Int, String) -> (Int, String)
induvidualizeTestcase  filename (n, str) =
    (n, "# Testcase " ++ show n ++ "\n" ++
        str ++
        "\ngenerate(\"" ++
        filename ++ "_" ++ show n ++".ssd\");")
        
mkFile :: String -> (Int, String) -> IO ()  
mkFile filename (n, str) = do
    outFile <- openFile (filename ++ "_" ++ show n ++ ".rb") WriteMode
    hPutStr outFile str
    hClose outFile   
    

-- | Takes the testcases in '[String]' and prints the
-- strings in a subdirectory called as the executable, which
-- calls this function, the name stripped after the first ".".    
printTestcases :: [String] -> IO ()
printTestcases testcases = do 
    putStr $ show (length testcases) ++ " Testfälle\n"
    let numberedTestcases = snd $ mapAccumL (\ n str -> (n+1, (n,str))) 1 testcases
    testgraphnameCompl <- getProgName
    let testgraphname = fst $ span  (/='.') testgraphnameCompl
    let idTestcases = map (induvidualizeTestcase testgraphname) numberedTestcases
    createDirectoryIfMissing False testgraphname
    let testgraphpath = testgraphname ++ "/" ++ testgraphname
    mapM_ (mkFile testgraphpath ) idTestcases

-- | Prints the testgraph as *.dot and *.ps  in the subdirectory  
printTestgraph :: GraphvizParams Node (Maybe a, Maybe TGDocuInfo) () () (Maybe a, Maybe TGDocuInfo) 
                 -> (VizGraph a) 
                 -> IO ()
printTestgraph view vizGraph = do
    let myVis = \graph -> graphToDot view graph
    let code = L.unpack $ printDotGraph (myVis vizGraph)
    testgraphnameCompl <- getProgName
    let testgraphname = fst $ span  (/='.') testgraphnameCompl
    let testgraphpath = testgraphname ++ "/" ++ testgraphname
    outGraphFile <- openFile (testgraphpath ++".dot") WriteMode
    hPutStr  outGraphFile code 
    hClose outGraphFile
    callCommand ( "dot -Tsvg -o " ++ testgraphpath ++".svg " ++
                                    testgraphpath ++".dot")
                                    
-- | prints the testgraph as *.dot and *.ps with name and path
printTestgraphP :: GraphvizParams Node (Maybe a, Maybe TGDocuInfo) () () (Maybe a, Maybe TGDocuInfo) 
                  -> (VizGraph a)
                  -> String
                  -> String 
                  -> IO ()
printTestgraphP view vizGraph path name = do
    let myVis = \graph -> graphToDot view graph
    let code = L.unpack $ printDotGraph (myVis vizGraph)
    let testgraphpath = path ++ "/" ++ name
    createDirectoryIfMissing True path
    outGraphFile <- openFile (testgraphpath ++".dot") WriteMode
    hPutStr  outGraphFile code 
    hClose outGraphFile
    callCommand ( "dot -Tsvg -o " ++ testgraphpath ++".svg " ++
                                    testgraphpath ++".dot")


