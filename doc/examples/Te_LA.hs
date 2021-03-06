{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, OverloadedStrings #-}


-- This is an example of the domain train control.
-- You need not to understand every requirement,
-- But you should learn, how a testcase in TestExplode is build

module Te_LA where -- exports everything, thus import only qualified!


import TestExplode.TestExplode
import TestExplode.DirGraphCombine
import VizViews
import FinalIO


import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Text.Lazy as L


docuinfo = TGDocuInfo { descForTex = [qc|
                           Entering a Speed Rest (SR) by all possibile ways,
                           (checking the allowed Speed by driving faster:)
                           possibile ways :
                           GK 3 in SBE (entering SBE with all possibilites)
                           GK 3 in SB
                           GK2 by GKS
                           GK2 by Infill
                           GK2 behind SB
                           Bhf's LA by GKS
                           Bhf's LA by Infill|],
                        name="LA_1",
                        descForNode = "Entering a Speed Rest\n\
                                      \with all possibilities",
                        generic = True,
                        toExpand =True
                       }


testgraph = Testgraph { dirGraph = dirgraph,
                        docuInfo = docuinfo
                      }
-- -------
-- Definition of the test data
-- -------


data TeLACnf = 
     TeLACnf { allowedSpeed :: Int
               ,markName :: String
             }
       deriving (Show)
       
teLATestset = [ TeLACnf { allowedSpeed = v,
                          markName = "In_SR"
                        }
                        |
                v <- [5, 10, 15]
               ] 

data TeLAVars =
     TeLAVars { restOfRest :: Int }

                      
-- ------
-- Definition of the dirGraph:
-- -----

                    
-- Parts of testcases (Casepart's)

-- Power Up

startRunCode :: TeLACnf -> TeLAVars -> L.Text
startRunCode cfg locals = [qc|
  switchOn();
  testTheUnit(ok);
  accelerateTo({(allowedSpeed cfg) -2});
|]

startRunCp = emptyCp 
             { shortDesc = "Start up and run",
               longDesc = "Switch the unit on\n Make functional test\n Accelerate below allowed speed",
               codeFkt = startRunCode
             }
             
-- SBE by GKS GK1

sbeByGKS1Code :: TeLACnf -> TeLAVars -> L.Text
sbeByGKS1Code cnf  locals = [qc|
  sendGKS(1, \{"SBE" =>1, "Z"=>20, "VZ"=> 80\});
  goDistance(25);|]
  
  
sbeByGKS1Cp = emptyCp
          { shortDesc = "Reach SBE with\n GK1: SBE, Z=20, VZ=80\n 5 m behind Z",
            codeFkt = sbeByGKS1Code
          }
-- SBe by end of Stoerfahrtweglaenge
         
sbeByUeStoerCode cfg locals = [qc|
  makeFue(1);
  godistance(6);
  makeFue(0);
  goDistance(1010);|]
  
sbeByUeStoerCp = emptyCp
          { shortDesc = "Failure in Transmission,\nRide",
            longDesc = "Fue, No Fue, 1010 m ride",
            codeFkt = sbeByUeStoerCode
          }
          
-- receive GK3, enter LA

sendGK3code :: TeLACnf -> TeLAVars -> L.Text
sendGK3code cfg locals = [qc|
  sendGKS(3, "ZLA"=> 20, "VLA"=>{ fromIntegral( allowedSpeed cfg) / 5 }, "LLA" => 500 );
  godistance(20); |]
  
sendGK3vars cfg locals = TeLAVars 480  

sendGK3Cp = emptyCp 
          { shortDesc ="Reach GK3-SR,\n 50 m until Z reached",
           codeFkt = sendGK3code,
            varFkt = sendGK3vars
          }
          
-- enter SB
enterSBcode :: TeLACnf -> TeLAVars -> L.Text
enterSBcode cfg locals = [qc|
  sendGKS(1, \{"Z"=> 3000\});
  godistance(10);|]

  
enterSBCp = emptyCp {
  shortDesc = "Enter Signal Run",
  codeFkt = enterSBcode
  }
  
-- makeMark

makeMark = emptyCp {
         shortDesc = "Mark",
         longDesc  ="Mark",
         codeFkt = \cfg locals -> [qc|
  setCheckMark({markName cfg});|]
         }
  
-- check V

checkVcodeCp = emptyCp {
             shortDesc = "VLA (V of Speed rest)\n above and down again",
             codeFkt = \cfg locals -> [qc|
  testZBv({allowedSpeed cfg});|]
             }

-- Run till end of speed rest     

endLACp = emptyCp {
          shortDesc = "leaving SR and testing V",
           codeFkt = \cfg locals -> [qc|
  godistance ({(restOfRest locals) -1});
  checkV(20);
  godistance (20);
  checkV(80);|]
          }
  
                    
-- the complete graph of the part-testcases   
               

dirgraph = mkEle startRunCp
            &-&
            split [ mkEle sbeByUeStoerCp,
                    mkEle sbeByGKS1Cp,
                    mkEle enterSBCp                  
                  ]
            &-&
            mkEle sendGK3Cp
            &-&
            mkEle (markCp "Mark_1")
            &-&
            mkEle checkVcodeCp
            &-&
            mkEle endLACp



                        

           
main :: IO ()
main = do 
     let testcases = generate "# " teLATestset (TeLAVars 0) (L.pack . show) (dirGraph testgraph)
     printTestcases testcases
     let vizGraph = mkVizGraph (dirGraph testgraph)
     printTestgraph recordView1 vizGraph

    
