{-# LANGUAGE OverloadedStrings #-}

{-|
Module : FinalIO
Description : The Final Functions for the end user. Writes many files.
Copyright : (c) Hans-Jürgen Guth, 2014
License : LGPL
Maintainer : juergen.software@freea2a.de
Stability : experimental

This Functions generate the testcases as executables and as visualisation.
-}

module FinalIO (printTestcases, printTestgraph, printTestgraphP) where

import TestExplode.DirGraphCombine (VizGraph)
import TestExplode.TestExplode  (TGDocuInfo)

import Data.List

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz

import qualified Data.Text.Lazy as L

import System.IO
import System.Process
import System.Environment
import System.Directory

-- | Change this function, if you need or like
induvidualizeTestcase :: L.Text   -- ^ the filename of the testcase 
                         -> (Int, L.Text) -- ^ (number of the testcase, 
                                          --    testcase)
                         -> (Int, L.Text) -- ^ (number of the testcase,
                                          --    testcase with header and footer) 
induvidualizeTestcase  filename (n, str) =
    (n, L.concat["# Testcase ", L.pack (show n), "\n",
                 str,
                 L.pack "\ngenerate(\"",
                 filename, "_", L.pack (show n), ".ssd\");"])


-- | here you can change the extension of the filename of the testcase        
mkFile :: L.Text -> (Int, L.Text) -> IO ()  
mkFile filename (n, str) = do
    outFile <- openFile (L.unpack((L.concat[filename,"_", L.pack (show n), ".rb"]))) WriteMode
    hPutStr outFile (L.unpack str)
    hClose outFile   
    

-- | Takes the testcases in '[String]' and prints the
-- strings in a subdirectory called as the executable, which
-- calls this function, the name stripped after the first ".".    
printTestcases :: [L.Text] -> IO ()
printTestcases testcases = do 
    putStr $ show (length testcases) ++ " testcases\n"
    -- mapAccumL is a bit complicated,
    -- numberedTestcases is a structure of [(1, testcase 1), 
    --                                      (2, testcase 2),
    --                                       ..
    --                                      (n, testcase n)] 
    let numberedTestcases = snd $ mapAccumL (\ n str -> (n+1, (n,str))) 1 testcases
    testgraphnameCompl <- getProgName
    let testgraphname = L.pack $ takeWhile (/='.') testgraphnameCompl
    let idTestcases = map (induvidualizeTestcase testgraphname) numberedTestcases
    createDirectoryIfMissing False (L.unpack testgraphname)
    let testgraphpath = L.concat [testgraphname,"/",testgraphname]
    mapM_ (mkFile testgraphpath ) idTestcases

-- | Prints the testgraph as *.dot and *.svg  in the subdirectory  
printTestgraph :: GraphvizParams Node (Maybe a, Maybe TGDocuInfo) () () (Maybe a, Maybe TGDocuInfo) -- ^ The GraphvizParams
                 -> (VizGraph a) -- ^ The VizGraph as input
                 -> IO ()        -- ^ the result is 'IO'
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
                                    
-- | prints the testgraph as *.dot and *.svg with name and path
printTestgraphP :: GraphvizParams Node (Maybe a, Maybe TGDocuInfo) () () (Maybe a, Maybe TGDocuInfo) -- ^ The GraphvizParams
                  -> (VizGraph a) -- ^ The VizGraph as input
                  -> String -- ^ the path to the file that shall be generated
                  -> String -- ^ the filename of the file that shall be generated
                  -> IO ()  -- ^ the result is 'IO'
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


