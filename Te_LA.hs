{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Te_LA where -- exports everything, thus import only qualified!


import TestExplode3
import DirGraphCombine
import VizViews
import FinalIO


import Text.InterpolatedString.Perl6 (qc)

-- Entering a Speed Rest with all possibilities,
-- (checking the allowed Speed by driving faster:) next Step
-- possibilites:
-- GK 3 in SBE (entering SBE with all possibilites)
-- GK 3 in SB
-- GK2 by GKS
-- GK2 by Infill
-- GK2 behind SB
-- Bhf's LA by GKS
-- Bhf's LA by Infill


data TeLACnf = 
     TeLACnf {  restOfRest :: Int
               ,allowedSpeed :: Int
               ,markName :: String
             }
       deriving (Show)
       
teLATestset = [ TeLACnf { restOfRest = 50,
                          allowedSpeed = v,
                          markName = "InLA"
                        }
                        |
                v <- [5, 10, 15]
               ] 

                    
-- Parts of testcases (Casepart's)

-- Power Up

startRunCode :: TeLACnf -> String
startRunCode cfg = [qc|
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

sbeByGKS1Code :: TeLACnf -> String
sbeByGKS1Code cnf  = [qc|
  sendGKS(1, \{"SBE" =>1, "Z"=>20, "VZ"=> 80\});
  goDistance(25);|]
  
sbeByGKS1Cp = emptyCp
          { shortDesc = "Reach SBE with\n GK1: SBE, Z=20, VZ=80\n 5 m behind Z",
            longDesc = "Reach SBE with\n GK1: SBE, Z=20, VZ=80\n 5 m behind Z",
            codeFkt = sbeByGKS1Code
          }
-- SBe by end of Stoerfahrtweglaenge
         
sbeByUeStoerCode cfg = [qc|
  makeFue(1);
  godistance(6);
  makeFue(0);
  goDistance(1010);|]
  
sbeByUeStoerCp = emptyCp
          { shortDesc = "Ue-Stoer, Weg abfahren",
            longDesc = "Fue, No Fue, 1010 m ride",
            codeFkt = sbeByUeStoerCode
          }
          
-- receive GK3, enter LA


sendGK3code cfg = [qc|
  sendGKS(3, "ZLA"=> 20, "VLA"=>{ fromIntegral( allowedSpeed cfg) / 5 }, "LLA" => 50 + { restOfRest cfg});
  godistance(20); |]

sendGK3Cp = emptyCp 
          { shortDesc ="GK3-LA erreichen,\n 50 m rest bis Ziel erreicht",
            longDesc =  "GK3-LA erreichen,\n 50 m rest bis Ziel erreicht",
            codeFkt = sendGK3code
          }
          
-- enter SB

enterSBcode cfg = [qc|
  sendGKS(1, \{"Z"=> 3000\});
  godistance(10);|]
  
enterSBCp = emptyCp {
  shortDesc = "Enter Signal Run",
  longDesc = "Enter signal run",
  codeFkt = enterSBcode
  }
  
-- makeMark

makeMark = emptyCp {
         shortDesc = "Mark",
         longDesc  ="Mark",
         codeFkt = \cfg -> [qc|
  setCheckMark({markName cfg});|]
         }
  
-- check V

checkVcodeCp = emptyCp {
             shortDesc = "VLA über- und wieder\nunterschreiten",
             longDesc = "VLA über- und wieder\nunterschreiten",
             codeFkt = \cfg -> [qc|
  testZBv({allowedSpeed cfg});|]
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


docuinfo = TGDocuInfo { descForTex = [qc|
Entering a Speed Rest with all possibilities,
(checking the allowed Speed by driving faster:) next Step
possibilites:
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
                        generic = False,
                        toExpand =True
                       }


testgraph = Testgraph { dirGraph = dirgraph,
                        docuInfo = docuinfo
                      }
                        

           
main :: IO ()
main = do 
     let testcases = generate teLATestset show (dirGraph testgraph)
     printTestcases testcases
     let vizGraph = mkVizGraph (dirGraph testgraph)
     printTestgraph recordView1 vizGraph

    
