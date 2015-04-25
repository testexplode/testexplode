{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, OverloadedStrings #-}

module Te52 where -- exports everything, thus import only qualified!

import qualified Te_LA 

import TestExplode.TestExplode
import TestExplode.DirGraphCombine
import VizViews
import FinalIO


import Text.InterpolatedString.Perl6 (qc)
import qualified Data.Text.Lazy as L


-- failure of the Speed Measurement, a short and a long time


data Te52Cnf = 
     Te52Cnf {  durationOfFailure :: Int
               ,speed :: Int
               ,percentageOfFailure :: Int
               ,wisirNo :: Int
             }
       deriving (Show)

data Te52Locals =
     Te52Locals { currTime     :: Int, -- ms
                  currDistance :: Int -- cm
                }
       deriving (Show)
                
startLocals = Te52Locals 0 0
        
te52Testset = [ Te52Cnf { durationOfFailure = dura,
                          speed = v,
                          percentageOfFailure = perc,
                          wisirNo = wiNo 
                        }
                        |
                dura <- [0,100], --,200,999, 1001, 5000],
                v <- [1, 10], --, 50,100],
                perc <- [0,9,11], --9,11, 50, 100],
                wiNo <- [1,2]
               ] 

                    
-- Parts of testcases (Casepart's)

-- Power Up

startRunCode :: Te52Cnf -> Te52Locals-> L.Text
startRunCode cfg locals = [qc|
  switchOn();
  testTheUnit(ok);
  accelerateTo({speed cfg});
  wait(10000)
|]

startRunVars :: Te52Cnf -> Te52Locals -> Te52Locals
startRunVars cnf locals = locals{currTime     = (currTime locals) + 10000,
                                 currDistance = (speed cnf) * 900 + (currDistance locals)
                                }

startRunCp = emptyCp 
             { shortDesc = "Start up and run",
               longDesc = "Switch the unit on\n Make functional test\n Accelerate to target speed",
               codeFkt = startRunCode,
               varFkt = startRunVars
             }

-- Difference of the Speed measurement
             
makeDifferenceCode :: Te52Cnf -> Te52Locals -> L.Text
makeDifferenceCode cfg locals = [qc|
   # states of the variables: {locals}
   wisirSpeed({wisirNo cfg}, { fromIntegral (speed cfg)* (1-(fromIntegral (percentageOfFailure cfg)/100.0))} );
   wait({durationOfFailure cfg});
|]


makeDifferenceVars cnf locals = 
   locals{currTime = round $ fromIntegral (currTime locals) * fromIntegral (percentageOfFailure cnf)/100.0}


makeDifferenceCp = emptyCp
             { shortDesc = "The 2 Speed measurements differ \n(or not)\nfor the given time",
               longDesc = "The 2 Speed measurements differ \n(or not)\nfor the given time",
               codeFkt = makeDifferenceCode,
               varFkt = makeDifferenceVars
             }

-- Check: Error!
  
checkNotOKCode :: Te52Cnf -> Te52Locals -> L.Text
checkNotOKCode cfg locals = [qc|
   # states of the variables: {locals}
   checkFailure(400, 208);
|]


checkNotOkCondition :: Te52Cnf -> Te52Locals -> Bool
checkNotOkCondition cfg locals = (percentageOfFailure cfg) > 10 && 
                          (durationOfFailure cfg) > 1000

checkNotOkCp = emptyCp
             { shortDesc = "Check: Failure",
               longDesc =  "Check: Failure",
               codeFkt = checkNotOKCode,
               condition = checkNotOkCondition,
               condDesc = "> 10 %, > 1 s"
             }
             
-- Check: No error
            
checkOKCode :: Te52Cnf -> Te52Locals -> L.Text
checkOKCode cfg locals = [qc|
   # states of the variables: {locals}
   checkFailure(None);
|]

checkOkCondition :: Te52Cnf -> Te52Locals -> Bool
checkOkCondition cfg locals = not (checkNotOkCondition cfg locals ) 

checkOkCp = emptyCp
             { shortDesc = "Check: No failure",
               longDesc =  "Check: No failure",
               codeFkt = checkOKCode,
               condition = checkOkCondition,
               condDesc = "not (> 10 %, > 1 s)"
             }

-- the special conversion functions

toTeLACnf :: Te52Cnf -> Te_LA.TeLACnf
toTeLACnf te52 = Te_LA.TeLACnf {Te_LA.allowedSpeed = (speed te52),
                                Te_LA.markName = "Mark_te52.1"
                               } 
useVarsTeLA :: Te52Locals -> Te_LA.TeLAVars
useVarsTeLA te52 = Te_LA.TeLAVars 0

takeVarsTeLA :: Te52Locals -> Te_LA.TeLAVars -> Te52Locals
takeVarsTeLA fromMaster fromEmbedded = 
               fromMaster{ currDistance = (currDistance fromMaster)
                                           + (1000 - (Te_LA.restOfRest fromEmbedded))
                         }
                
-- the complete graph of the part-testcases   
               

testgraph = split [ mkEle startRunCp,
                    mkGraph2Ele toTeLACnf useVarsTeLA takeVarsTeLA NotExpand Te_LA.testgraph
                  ]
            &-&
            mkEle makeDifferenceCp
            &-&
            split [ mkEle checkNotOkCp,
                    mkEle checkOkCp
                  ]
 


printSubGraph :: String -> (String, Testgraph (CasepartInternal cnf locals )) -> IO ()
printSubGraph path (name, testgraph) = 
    let vizGraph = mkVizGraph (dirGraph testgraph)
    in
    printTestgraphP recordView1 vizGraph path name 

           
main :: IO ()
main = do 
     let testcases = generate "# " te52Testset startLocals (L.pack . show) testgraph
     printTestcases testcases
     let vizGraph = mkVizGraph testgraph
     printTestgraph recordView1 vizGraph
     -- Print the subgraphs
     let subGraphList = getSubGraphs testgraph []
     -- putStr $ show $ length subGraphList
     -- mapM_ (putStr . show . fst) subGraphList
     mapM_ (printSubGraph "subgraphs") subGraphList  

    
