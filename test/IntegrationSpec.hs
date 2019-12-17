module IntegrationSpec (spec) where

import  Test.Hspec
import  Data.Name
import System.IO.Unsafe

sampleRegistry = unsafePerformIO $ do
  names <- fmap lines (readFile "test/data/givens.txt")
  surnames <- fmap lines (readFile "test/data/families.txt")
  return $ makeRegistry names surnames (defaultOptions { transliterateNames = True, treatUnknownAsFamily = True })

run = fix sampleRegistry
runMaybe = fixMaybe sampleRegistry

spec :: Spec
spec = do
  describe "fix" $ do
    describe "GivenAndFamily" $ do

      it "ś ń" $ run (GivenAndFamily ["cannavo"] ["monica"]) `shouldBe` (GivenAndFamily ["monica"] ["cannavo"])
      it "N S" $ run (GivenAndFamily ["Rocío"] ["Gonzalez"]) `shouldBe` (GivenAndFamily ["Rocío"] ["Gonzalez"])
      it "S N" $ run (GivenAndFamily ["Calvo"] ["Felipe"]) `shouldBe` (GivenAndFamily ["Felipe"] ["Calvo"])

      it "NN S" $ run (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "S NN" $ run (GivenAndFamily ["Scarpa"] ["Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])

      it "N SS" $ run (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SS N" $ run (GivenAndFamily ["Feldfeber", "Kivelsky"] ["Ivana"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])

      it "S S" $ runMaybe (GivenAndFamily ["Bulgarelli"] ["Alt"]) `shouldBe` Nothing

      it "N N" $ runMaybe (GivenAndFamily ["Ivana"] ["Giselle"]) `shouldBe` Nothing

      it "A N" $ run (GivenAndFamily ["Laura"] ["Giselle"]) `shouldBe` (GivenAndFamily ["Giselle"] ["Laura"])
      it "N A" $ run (GivenAndFamily ["Giselle"] ["Laura"]) `shouldBe` (GivenAndFamily ["Giselle"] ["Laura"])

      it "SS A" $ run (GivenAndFamily ["Villani", "Trucco"] ["Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]

    describe "FullName" $ do

      it "NS" $ run (FullName ["Rocío", "Gonzalez"]) `shouldBe` (GivenAndFamily ["Rocío"] ["Gonzalez"])
      it "SN" $ run (FullName ["Calvo", "Felipe"]) `shouldBe` (GivenAndFamily ["Felipe"] ["Calvo"])

      it "NNS" $ run (FullName ["Federico", "Alfredo", "Scarpa"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "SNN" $ run (FullName ["Scarpa", "Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Alfredo"] ["Scarpa", "Federico"])

      it "NSS" $ run (FullName ["Ivana", "Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SSN" $ run (FullName ["Feldfeber", "Kivelsky", "Ivana"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])

      it "SS" $ runMaybe (FullName ["Bulgarelli", "Alt"]) `shouldBe` Nothing

      it "NN" $ runMaybe (FullName ["Ivana", "Giselle"]) `shouldBe` Nothing

      it "AN" $ run (FullName ["Laura", "Giselle"]) `shouldBe` (GivenAndFamily ["Giselle"] ["Laura"])
      it "NA" $ run (FullName ["Giselle", "Laura"]) `shouldBe` (GivenAndFamily ["Giselle"] ["Laura"])

      it "SSA" $ run (FullName ["Villani", "Trucco", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]


