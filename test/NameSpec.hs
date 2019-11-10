module NameSpec (spec) where

import  Test.Hspec
import  Data.Name

sampleBag = Bag ["Daniela", "Federico", "Juan", "José", "Ana", "Gonzalo", "Rodrigo"] ["Scarpa", "Villani", "Tata", "Rodrigo", "Gonzalo"]
run = fix sampleBag

spec :: Spec
spec = do
  describe "fix" $ do
    it "N S" $ run (NameAndSurname ["Federico"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico"]) (Sure ["Scarpa"]))
    it "S N" $ run (NameAndSurname ["Villani"] ["Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Sure ["Villani"]))
    it "O O" $ run (NameAndSurname ["Rodrigo"] ["Alfonso"]) `shouldBe` (FixedName (Unsure ["Rodrigo"]) (Unsure ["Alfonso"]))
    it "O O" $ run (NameAndSurname ["Alfonso"] ["Rodrigo"]) `shouldBe` (FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo"]))

