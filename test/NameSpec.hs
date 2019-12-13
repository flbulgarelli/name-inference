module NameSpec (spec) where

import  Test.Hspec
import  Data.Name

sampleBag = Bag names surnames
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

run = fix sampleBag

spec :: Spec
spec = do
  describe "fix" $ do
    it "N S" $ run (NameAndSurname ["Rocío"] ["Gonzalez"]) `shouldBe` (FixedName (Sure ["Rocío"]) (Sure ["Gonzalez"]))
    it "S N" $ run (NameAndSurname ["Calvo"] ["Felipe"]) `shouldBe` (FixedName (Sure ["Felipe"]) (Sure ["Calvo"]))

    it "NN S" $ run (NameAndSurname ["Federico", "Alfredo"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico", "Alfredo"]) (Sure ["Scarpa"]))
    it "S NN" $ run (NameAndSurname ["Scarpa"] ["Federico", "Alfredo"]) `shouldBe` (FixedName (Sure ["Federico", "Alfredo"]) (Sure ["Scarpa"]))

    it "N SS" $ run (NameAndSurname ["Ivana"] ["Feldfeber", "Kivelsky"]) `shouldBe` (FixedName (Sure ["Ivana"]) (Sure ["Feldfeber", "Kivelsky"]))
    it "SS N" $ run (NameAndSurname ["Feldfeber", "Kivelsky"] ["Ivana"]) `shouldBe` (FixedName (Sure ["Ivana"]) (Sure ["Feldfeber", "Kivelsky"]))

    it "S S" $ run (NameAndSurname ["Bulgarelli"] ["Alt"]) `shouldBe` (FixedName Impossible Impossible)

    it "N N" $ run (NameAndSurname ["Laura"] ["Giselle"]) `shouldBe` (FixedName Impossible Impossible)

    it "A A" $ run (NameAndSurname ["Rodrigo"] ["Alfonso"]) `shouldBe` FixedName (Unsure ["Rodrigo"]) (Unsure ["Alfonso"])
    it "A A" $ run (NameAndSurname ["Alfonso"] ["Rodrigo"]) `shouldBe` FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo"])

    it "A AS" $ run (NameAndSurname ["Alfonso"] ["Rodrigo", "Trucco"] ) `shouldBe` FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo", "Trucco"])
    it "AS A" $ run (NameAndSurname ["Rodrigo", "Trucco"] ["Alfonso"]) `shouldBe` FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo", "Trucco"])

    it "A SA" $ run (NameAndSurname ["Alfonso"] ["Pina", "Rodrigo"] ) `shouldBe` FixedName (Unsure ["Alfonso"]) (Unsure ["Pina", "Rodrigo"])
    it "SA A" $ run (NameAndSurname ["Pina", "Rodrigo"] ["Alfonso"]) `shouldBe` FixedName (Unsure ["Alfonso"]) (Unsure ["Pina", "Rodrigo"])

    it "AN AS" $ run (NameAndSurname ["Alfonso", "Julián"] ["Rodrigo", "Trucco"] ) `shouldBe` FixedName (Unsure ["Alfonso", "Julián"]) (Unsure ["Rodrigo", "Trucco"])
    it "AS AN" $ run (NameAndSurname ["Rodrigo", "Trucco"] ["Alfonso", "Julián"]) `shouldBe` FixedName (Unsure ["Alfonso", "Julián"]) (Unsure ["Rodrigo", "Trucco"])

    it "NA SA" $ run (NameAndSurname ["Leonardo", "Alfonso"] ["Finzi", "Rodrigo"] ) `shouldBe` FixedName (Unsure ["Leonardo", "Alfonso"]) (Unsure ["Finzi", "Rodrigo"])
    it "SA NA" $ run (NameAndSurname ["Finzi", "Rodrigo"] ["Leonardo", "Alfonso"]) `shouldBe` FixedName (Unsure ["Leonardo", "Alfonso"]) (Unsure ["Finzi", "Rodrigo"])

    it "A SS" $ run (NameAndSurname ["Alfonso"] ["Villani", "Trucco"] ) `shouldBe` FixedName (Unsure ["Alfonso"]) (Sure ["Villani", "Trucco"])
    it "SS A" $ run (NameAndSurname ["Villani", "Trucco"] ["Alfonso"]) `shouldBe` FixedName (Unsure ["Alfonso"]) (Sure ["Villani", "Trucco"])

    it "NN AS" $ run (NameAndSurname ["Nadia", "Rocío"] ["Rodrigo", "Trucco"] ) `shouldBe` FixedName (Sure ["Nadia", "Rocío"]) (Unsure ["Rodrigo", "Trucco"])
    it "AS NN" $ run (NameAndSurname ["Rodrigo", "Trucco"] ["Nadia", "Rocío"]) `shouldBe` FixedName (Sure ["Nadia", "Rocío"]) (Unsure ["Rodrigo", "Trucco"])

