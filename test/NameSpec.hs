module NameSpec (spec) where

import  Test.Hspec
import  Data.PersonalName

sampleRegistry = makeRegistry names surnames (defaultOptions { transliterateNames = True })
  where
    names = [
      "Franco",
      "Leonardo",
      "Agustín",
      "Federico",
      "Alfredo",
      "Laura",
      "Mónica",
      "Judith",
      "Nadia",
      "Giselle",
      "Julián",
      "Luis",
      "Tomás",
      "Rocío",
      "Carolina",
      "Luisa",
      "Gustavo",
      "Ernesto",
      "Ivana",
      "Daniela",
      "Felipe",
      "Andres",
      "Daniela",
      "Veronica",
      "Rodrigo",
      "Alfonso"
      ]

    surnames = [
      "Bulgarelli",
      "Pina",
      "Scarpa",
      "Mangifesta",
      "Gruszczanski",
      "Finzi",
      "Berbel",
      "Alt",
      "Cannavó",
      "Gonzalez",
      "Baldino",
      "Trucco",
      "Feldfeber",
      "Kivelsky",
      "Szklanny",
      "Calvo",
      "Villani",
      "Alfonso",
      "Rodrigo"
      ]

run = fix sampleRegistry justBreakNames
runWith bonus = fix sampleRegistry (justBreakNamesWith bonus)
runMaybe = fixMaybe sampleRegistry splitNames

spec :: Spec
spec = do
  describe "fix" $ do
    describe "GivenAndFamily" $ do

      it "N S" $ run (GivenAndFamily ["Rocío"] ["Gonzalez"]) `shouldBe` (GivenAndFamily ["Rocío"] ["Gonzalez"])
      it "S N" $ run (GivenAndFamily ["Calvo"] ["Felipe"]) `shouldBe` (GivenAndFamily ["Felipe"] ["Calvo"])

      it "NN S" $ run (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "S NN" $ run (GivenAndFamily ["Scarpa"] ["Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])

      it "N SS" $ run (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SS N" $ run (GivenAndFamily ["Feldfeber", "Kivelsky"] ["Ivana"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])

      it "[S S]" $ runMaybe (GivenAndFamily ["Bulgarelli"] ["Alt"]) `shouldBe` Nothing

      it "[N N]" $ runMaybe (GivenAndFamily ["Laura"] ["Giselle"]) `shouldBe` Nothing

      it "A A" $ run (GivenAndFamily ["Rodrigo"] ["Alfonso"]) `shouldBe` GivenAndFamily ["Rodrigo"] ["Alfonso"]
      it "A A" $ run (GivenAndFamily ["Alfonso"] ["Rodrigo"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo"]

      it "A AS" $ run (GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]
      it "AS A" $ run (GivenAndFamily ["Rodrigo", "Trucco"] ["Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]

      it "A SA" $ run (GivenAndFamily ["Alfonso"] ["Pina", "Rodrigo"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Pina", "Rodrigo"]
      it "SA A" $ run (GivenAndFamily ["Pina", "Rodrigo"] ["Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Pina", "Rodrigo"]

      it "AN AS" $ run (GivenAndFamily ["Alfonso", "Julián"] ["Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso", "Julián"] ["Rodrigo", "Trucco"]
      it "AS AN" $ run (GivenAndFamily ["Rodrigo", "Trucco"] ["Alfonso", "Julián"]) `shouldBe` GivenAndFamily ["Alfonso", "Julián"] ["Rodrigo", "Trucco"]

      it "NA SA" $ run (GivenAndFamily ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"]) `shouldBe` GivenAndFamily ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"]
      it "SA NA" $ run (GivenAndFamily ["Finzi", "Rodrigo"] ["Leonardo", "Alfonso"]) `shouldBe` GivenAndFamily ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"]

      it "A SS" $ run (GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]
      it "SS A" $ run (GivenAndFamily ["Villani", "Trucco"] ["Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]

      it "NN AS" $ run (GivenAndFamily ["Nadia", "Rocío"] ["Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Nadia", "Rocío"] ["Rodrigo", "Trucco"]
      it "AS NN" $ run (GivenAndFamily ["Rodrigo", "Trucco"] ["Nadia", "Rocío"]) `shouldBe` GivenAndFamily ["Nadia", "Rocío"] ["Rodrigo", "Trucco"]

      it "nn s" $ run (GivenAndFamily ["carolina", "veronica"] ["gruszczanski"]) `shouldBe` GivenAndFamily ["carolina", "veronica"] ["gruszczanski"]
      it "s nn" $ run (GivenAndFamily ["gruszczanski"] ["carolina", "veronica"]) `shouldBe` GivenAndFamily ["carolina", "veronica"] ["gruszczanski"]

      it "Ń Ś" $ run (GivenAndFamily ["Monica"] ["Cannavo"]) `shouldBe` (GivenAndFamily ["Monica"] ["Cannavo"])
      it "Ś Ń" $ run (GivenAndFamily ["Cannavo"] ["Monica"]) `shouldBe` (GivenAndFamily ["Monica"] ["Cannavo"])

    describe "FullName" $ do

      it "NS" $ run (FullName ["Rocío", "Gonzalez"]) `shouldBe` (GivenAndFamily ["Rocío"] ["Gonzalez"])
      it "SN" $ run (FullName ["Calvo", "Felipe"]) `shouldBe` (GivenAndFamily ["Felipe"] ["Calvo"])

      it "NNS" $ run (FullName ["Federico", "Alfredo", "Scarpa"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "SNN" $ run (FullName ["Scarpa", "Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "SNN#familish" $ runWith bonusFamilish (FullName ["Scarpa", "Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])
      it "SNN#givenish" $ runWith bonusGivenish (FullName ["Scarpa", "Federico", "Alfredo"]) `shouldBe` (GivenAndFamily ["Federico", "Alfredo"] ["Scarpa"])

      it "NSS" $ run (FullName ["Ivana", "Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SSN" $ run (FullName ["Feldfeber", "Kivelsky", "Ivana"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SSN#familish" $ runWith bonusFamilish (FullName ["Ivana", "Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])
      it "SSN#givenish" $ runWith bonusGivenish (FullName ["Ivana", "Feldfeber", "Kivelsky"]) `shouldBe` (GivenAndFamily ["Ivana"] ["Feldfeber", "Kivelsky"])

      it "[SS]" $ runMaybe (FullName ["Bulgarelli", "Alt"]) `shouldBe` Nothing

      it "NN" $ runMaybe (FullName ["Laura", "Giselle"]) `shouldBe` Nothing

      it "AA" $ run (FullName ["Rodrigo", "Alfonso"]) `shouldBe` GivenAndFamily ["Rodrigo"] ["Alfonso"]
      it "AA" $ run (FullName ["Alfonso", "Rodrigo"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo"]

      it "ASA" $ run (FullName ["Rodrigo", "Trucco", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]
      it "ASA#familish" $ runWith bonusFamilish (FullName ["Rodrigo", "Trucco", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]
      it "ASA#givenish" $ runWith bonusGivenish (FullName ["Rodrigo", "Trucco", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]

      it "AAS" $ run (FullName ["Alfonso", "Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso", "Rodrigo"] ["Trucco"]
      it "AAS#familish" $ runWith bonusFamilish (FullName ["Alfonso", "Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Rodrigo", "Trucco"]
      it "AAS#givenish" $ runWith bonusGivenish (FullName ["Alfonso", "Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso", "Rodrigo"] ["Trucco"]

      it "SAA" $ run (FullName ["Pina", "Rodrigo", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Pina", "Rodrigo"]
      it "SAA#familish" $ runWith bonusFamilish (FullName ["Pina", "Rodrigo", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Pina", "Rodrigo"]
      it "SAA#givenish" $ runWith bonusGivenish (FullName ["Pina", "Rodrigo", "Alfonso"]) `shouldBe` GivenAndFamily ["Rodrigo", "Alfonso"] ["Pina"]

      it "ANAS" $ run (FullName ["Alfonso", "Julián", "Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso", "Julián", "Rodrigo"] ["Trucco"]
      it "ASAN" $ run (FullName ["Rodrigo", "Trucco", "Alfonso", "Julián"]) `shouldBe` GivenAndFamily ["Julián"] ["Rodrigo", "Trucco", "Alfonso"]

      it "NASA" $ run (FullName ["Leonardo", "Alfonso", "Finzi", "Rodrigo"]) `shouldBe` GivenAndFamily ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"]
      it "SANA" $ run (FullName ["Finzi", "Rodrigo", "Leonardo", "Alfonso"]) `shouldBe` GivenAndFamily ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"]

      it "[ANAS]" $ runMaybe (FullName ["Alfonso", "Julián", "Rodrigo", "Trucco"]) `shouldBe` Nothing
      it "[ASAN]" $ runMaybe (FullName ["Rodrigo", "Trucco", "Alfonso", "Julián"]) `shouldBe` Nothing

      it "[ANSAS]" $ runMaybe (FullName ["Alfonso", "Julián", "Berbel", "Rodrigo", "Trucco"]) `shouldBe` Just (GivenAndFamily ["Alfonso","Julián"] ["Berbel","Rodrigo","Trucco"])
      it "[ASNAN]" $ runMaybe (FullName ["Rodrigo", "Trucco", "Luis", "Alfonso", "Julián"]) `shouldBe` Just (GivenAndFamily ["Luis", "Alfonso","Julián"] ["Rodrigo","Trucco"])

      it "[NASA]" $ runMaybe (FullName ["Leonardo", "Alfonso", "Finzi", "Rodrigo"]) `shouldBe` Nothing
      it "[SANA]" $ runMaybe (FullName ["Finzi", "Rodrigo", "Leonardo", "Alfonso"]) `shouldBe` Nothing

      it "ASS" $ run (FullName ["Alfonso", "Villani", "Trucco"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]
      it "SSA" $ run (FullName ["Villani", "Trucco", "Alfonso"]) `shouldBe` GivenAndFamily ["Alfonso"] ["Villani", "Trucco"]

      it "NNAS" $ run (FullName ["Nadia", "Rocío", "Rodrigo", "Trucco"]) `shouldBe` GivenAndFamily ["Nadia", "Rocío", "Rodrigo"] ["Trucco"]
      it "ASNN" $ run (FullName ["Rodrigo", "Trucco", "Nadia", "Rocío"]) `shouldBe` GivenAndFamily ["Nadia", "Rocío"] ["Rodrigo", "Trucco"]

      it "N" $ run (FullName ["Nadia"]) `shouldBe` (FullName ["Nadia"])
