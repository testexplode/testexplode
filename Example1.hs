{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, OverloadedStrings #-}

module Example1 where -- exports everything, thus import only qualified!

import qualified Te_LA2 as Te_LA

import TestExplode3
import DirGraphCombine
import VizViews
import FinalIO

import Text.InterpolatedString.Perl6 (qc)
import qualified Data.Text.Lazy as L

-- SIG=2 (take VGR immediately)

data Ex1Cnf = Ex1Cnf {vz1kmh  :: Int,
                      vgr2kmh :: Int
                     }
     deriving (Show)
                     
ex1Testset = [Ex1Cnf { vz1kmh  = vz,
                       vgr2kmh = vgr
                     } 
               | vz <- [0, 10, 30, 80],
                 vgr <- [10, 15, 30, 80]
              ]
              
data Ex1Hints = Ex1Hints {distanceToZ :: Int,  -- cm
                          currSpeed   :: Int   -- km/h
                         } 
   deriving (Show)
   
-- Power Up


initCp = emptyCp { shortDesc = "Power Up with special SSE-Data",
                   longDesc = "Power Up with special SSE-Data",
                   codeFkt = \cnf hints -> [qc|
ignoreErrorMessages([1,2,404]);
setDqTraceOn(["WSATP_01"]);
setXtracesOn([190, 8, 9, 212]);

hochlaufSSE("ZB"=>1000, "BB"=>1000, "BBA1"=>100, "ZBA1"=>100, 
            "SZKS"=> 20*100, "VMAX"=>100, "TYP"=>1000);
|]
                 }
                 
-- First GKS   


firstGKS = emptyCp { shortDesc = "Define first GKS with chosen VZ\nand VGR=30",
                     longDesc = "Define first GKS with chosen VZ\nand VGR=30",
                     codeFkt = \cnf hints -> [qc|
anfahrenKmH(20, 10); # to 10 km/h in 20 m

clearBalise();
addPacket(7, \{"VGR"=>30, "VZ"=>{vz1kmh cnf}, "Z"=>400\});
defineGKS("GKS1");
|],
                     varFkt = \cnf hints -> hints{currSpeed=10}
                   }
-- Mark


markGKS1 = markCp "GKS1"

-- Send GKS1


sendGKS = emptyCp { shortDesc = "Send first GKS and Check 30 km/h",
                    longDesc = "Send first GKS and Check 30 km/h",
                    codeFkt = \cnf hints -> [qc|
ueberfahreGKS("GKS1", 3);
godistance(20);
checkATPGeschwindigkeit(\{"Exaktwert", "Vsoll", 30 - 0.1, 30, "GKS1"\});
|],
                    varFkt = \cnf hints -> hints{distanceToZ = 400 -20 -3-2}
                  } 
                  
-- receive infill
infillCp = emptyCp { shortDesc = "Send infill with chosen VGR\nand SIG=2 and drive and check V",
                      longDesc = "Send infill with chosen VGR\nand SIG=2 and drive and check V",
                      codeFkt = \cnf hints -> [qc|
clearBalise();
addPacket(7, \{"VGR"=>{vgr2kmh cnf}, "VZ"=>0, "Z"=>1000, "SIG"=>2\});
defineGKS("infill");
# intern variables: {hints}
startFunkaufwertung("ZRADIOA", "infill");
godistance(20);
checkATPGeschwindigkeitT(\{"Exaktwert", "Vsoll", {max (30, vgr2kmh cnf) - 0.1, max (30, vgr2kmh cnf), 400\});
|],
                       varFkt = \cnf hints -> hints{distanceToZ = (distanceToZ hints) - 20 }
                     }

-- Mark
markInfill = markCp "Infill"


-- If downgrade, drive till begin of braking curve

-- Helper function, a pseudo-braking curve
way2Z :: Int -> Int -> Int -> Int
way2Z a b c = a - floor ( (fromIntegral (b-c))/ (2.0*3.6)) 


curveBeginCp = emptyCp{ shortDesc = "drive till begin and in \n braking curve and check",
                        longDesc = "drive till begin and in \n braking curve and check",
                        codeFkt = \cnf hints ->
let wayToDrive = (distanceToZ hints) - way2Z (distanceToZ hints) (vgr2kmh cnf) 30
in 
[qc|
# intern variables: {hints}
goDistance({wayToDrive}-1);
checkAtpGeschwindigkeit(\{"Exaktwert", "Vsoll", 30 - 0.1,30, "Infill"\});
godistance(2);
checkAtpGeschwindigkeitT(\{"Exaktwert", "Vsoll", 30 - 0.6,30-0.2 , 300\});
 
|],
                        varFkt = \cnf hints ->
                                  let wayToDrive = (distanceToZ hints) - way2Z (distanceToZ hints) (vgr2kmh cnf) 30
                                  in
                                  hints{distanceToZ = (distanceToZ hints) - wayToDrive -1},
                        condition = \cnf hints -> (vgr2kmh cnf) < 30,
                        condDesc = "VGR(Infill) < 30"
                      }
               

-- if upgrade, test emergency brake

upgradeCp = emptyCp{ shortDesc = "EB due to overspeed",
                     longDesc = "EB due to overspeed",
                     codeFkt = \cnf hints ->
[qc|
accelmeter({vgr2kmh cnf}, 30);
setMark("equalSpeed");
accelmeter({(vgr2kmh cnf + 1)}, 1);
godistance(5);
checkZB("Sollwert","on", "equalSpeed");
godistance({distanceToZ hints});
checkZB("Tabuwert","off", "equalSpeed");
|], 
                     condition = \cnf hints -> (vgr2kmh cnf) >= 30, 
                     condDesc = "VGR(Infill) >= 30"
                   }
                   
                   
-- the special conversion functions

toTeLACnf :: Ex1Cnf -> Te_LA.TeLACnf
toTeLACnf myCnf = Te_LA.TeLACnf {Te_LA.allowedSpeed = 25,
                                 Te_LA.markName = "Mark_SpeedRest"
                                } 
                               
useVarsTeLA :: Ex1Hints -> Te_LA.TeLAVars
useVarsTeLA myHint = Te_LA.TeLAVars 0

takeVarsTeLA :: Ex1Hints -> Te_LA.TeLAVars -> Ex1Hints
takeVarsTeLA myHint itsHint = 
                myHint{ distanceToZ = (distanceToZ myHint)
                                           + (Te_LA.runnedDistance itsHint)
                      }


-- the complete graph of the part-testcases  

testgraph = mkEle initCp
            &-&
            mkEle firstGKS
            &-&
            mkEle markGKS1
            &-&
            mkEle sendGKS
            &-&
            mkEle infillCp
            &-&
            mkEle markInfill
            &-&
            mkGraph2Ele toTeLACnf useVarsTeLA takeVarsTeLA NotExpand Te_LA.testgraph
            &-&
            split [mkEle curveBeginCp,
                   mkEle upgradeCp
                  ] 


printSubGraph :: String -> (String, Testgraph (CasepartInternal cnf locals )) -> IO ()
printSubGraph path (name, testgraph) = 
    let vizGraph = mkVizGraph (dirGraph testgraph)
    in
    printTestgraphP recordView1 vizGraph path name 

           
main :: IO ()
main = do 
     let testcases = generate ex1Testset (Ex1Hints 0 0) (L.pack . show) testgraph
     printTestcases testcases
     let vizGraph = mkVizGraph testgraph
     printTestgraph recordView1 vizGraph
     -- Print the subgraphs
     let subGraphList = getSubGraphs testgraph []
     mapM_ (printSubGraph "subgraphs") subGraphList  
