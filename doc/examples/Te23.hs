{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Te23 where -- exports everything, thus import only qualified!

import qualified Te_LA 

import TestExplode.TestExplode3
import TestExplode.DirGraphCombine
import VizViews
import FinalIO


import Text.InterpolatedString.Perl6 (qc)



-- failure of the Speed Measurement, a short and a long time


data Te23Cnf = 
     Te23Cnf {  durationOfFailure :: Int
               ,speed :: Int
               ,percentageOfFailure :: Int
               ,wisirNo :: Int
             }
       deriving (Show)
       
te23Testset = [ Te23Cnf { durationOfFailure = dura,
                          speed = v,
                          percentageOfFailure = perc,
                          wisirNo = wiNo 
                        }
                        |
                dura <- [0,100], --,200,999, 1001, 5000],
                v <- [1, 10], --, 50,100],
                perc <- [0,1], --9,11, 50, 100],
                wiNo <- [1,2]
               ] 

                    
-- Parts of testcases (Casepart's)

-- Power Up

startRunCode :: Te23Cnf -> String
startRunCode cfg = [qc|
  switchOn();
  testTheUnit(ok);
  accelerateTo({speed cfg});
  wait(10000)
|]

startRunCp = emptyCp 
             { shortDesc = "Start up and run",
               longDesc = "Switch the unit on\n Make functional test\n Accelerate to target speed",
               codeFkt = startRunCode
             }

-- Difference of the Speed measurement
             
makeDifferenceCode :: Te23Cnf -> String
makeDifferenceCode cfg = [qc|
   wisirSpeed({wisirNo cfg}, { fromIntegral (speed cfg)* (1-(fromIntegral (percentageOfFailure cfg)/100.0))} );
   wait({durationOfFailure cfg});
|]

makeDifferenceCp = emptyCp
             { shortDesc = "The 2 Speed measurements differ \n(or not)\nfor the given time",
               longDesc = "The 2 Speed measurements differ \n(or not)\nfor the given time",
               codeFkt = makeDifferenceCode
             }

-- Check: Error!
  
checkNotOKCode :: Te23Cnf -> String
checkNotOKCode cfg = [qc|
   checkFailure(400, 208);
|]


checkNotOkCondition :: Te23Cnf -> Bool
checkNotOkCondition cfg = (percentageOfFailure cfg) > 10 && 
                          (durationOfFailure cfg) > 1000

checkNotOkCp = emptyCp
             { shortDesc = "Check: Failure",
               longDesc =  "Check: Failure",
               codeFkt = checkNotOKCode,
               condition = checkNotOkCondition,
               condDesc = "> 10 %, > 1 s"
             }
             
-- Check: No error
            
checkOKCode :: Te23Cnf -> String
checkOKCode cfg = [qc|
   checkFailure();
|]

checkOkCondition :: Te23Cnf -> Bool
checkOkCondition cfg = not (checkNotOkCondition cfg) 

checkOkCp = emptyCp
             { shortDesc = "Check: No failure",
               longDesc =  "Check: No failure",
               codeFkt = checkOKCode,
               condition = checkOkCondition,
               condDesc = "not (> 10 %, > 1 s)"
             }

-- the special conversion function

toTeLA :: Te23Cnf -> Te_LA.TeLACnf
toTeLA te23 = Te_LA.TeLACnf {Te_LA.restOfRest = (durationOfFailure te23) * (speed te23) +10,
                             Te_LA.allowedSpeed = 10,
                             Te_LA.markName = "Mark_te23.1"
                            } 


                
-- the complete graph of the part-testcases   
               

testgraph = split [ mkEle startRunCp,
                    mkGraph2Ele toTeLA NotExpand Te_LA.testgraph
                  ]
            &-&
            mkEle makeDifferenceCp
            &-&
            split [ mkEle checkNotOkCp,
                    mkEle checkOkCp
                  ]
 


printSubGraph :: String -> (String, Testgraph (Casepart cnf)) -> IO ()
printSubGraph path (name, testgraph) = 
    let vizGraph = mkVizGraph (dirGraph testgraph)
    in
    printTestgraphP recordView1 vizGraph path name 

           
main :: IO ()
main = do 
     let testcases = generate "# " te23Testset show testgraph
     printTestcases testcases
     let vizGraph = mkVizGraph testgraph
     printTestgraph recordView1 vizGraph
     -- Print the subgraphs
     let subGraphList = getSubGraphs testgraph []
     -- putStr $ show $ length subGraphList
     -- mapM_ (putStr . show . fst) subGraphList
     mapM_ (printSubGraph "subgraphs") subGraphList  

    
