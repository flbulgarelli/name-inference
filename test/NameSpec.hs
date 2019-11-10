module NameSpec (spec) where

import  Test.Hspec
import  Data.Name

sampleBag = Bag ["Daniela", "Rocío", "Federico", "Juan", "María", "Beatriz", "Florencia", "Ana", "Gonzalo", "Rodrigo", "Franco", "Washignton"] ["Scarpa", "Villani", "Tata", "Rodrigo", "Gonzalo", "Finzi", "Rodríguez", "Washignton"]
run = fix sampleBag

spec :: Spec
spec = do
  describe "fix" $ do

    it "N S" $ run (NameAndSurname ["Federico"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico"]) (Sure ["Scarpa"]))
    it "S N" $ run (NameAndSurname ["Villani"] ["Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Sure ["Villani"]))
    it "O O" $ run (NameAndSurname ["Rodrigo"] ["Alfonso"]) `shouldBe` (FixedName (Unsure ["Rodrigo"]) (Unsure ["Alfonso"]))
    it "O O" $ run (NameAndSurname ["Alfonso"] ["Rodrigo"]) `shouldBe` (FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo"]))
    it "N O" $ run (NameAndSurname ["Rocío"] ["Rodrigo"]) `shouldBe` (FixedName (Sure ["Rocío"]) (Unsure ["Rodrigo"]))
    it "O N" $ run (NameAndSurname ["Carlos"] ["Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Unsure ["Carlos"]))
    it "O S" $ run (NameAndSurname ["Rodrigo"] ["Finzi"]) `shouldBe` (FixedName (Unsure ["Rodrigo"]) (Sure ["Finzi"]))
    it "S O" $ run (NameAndSurname ["Finzi"] ["Gonzalo"]) `shouldBe` (FixedName (Unsure ["Gonzalo"]) (Sure ["Finzi"]))

    it "NO S"  $ run (NameAndSurname ["Federico", "Rodrigo"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico", "Rodrigo"]) (Sure ["Scarpa"]))
    it "NO SS" $ run (NameAndSurname ["Federico", "Rodrigo"] ["Scarpa", "Rodríguez"]) `shouldBe` (FixedName (Sure ["Federico", "Rodrigo"]) (Sure ["Scarpa", "Rodríguez"]))
    it "NO SO" $ run (NameAndSurname ["Franco", "Washignton"] ["Tata", "Gonzalo"]) `shouldBe` (FixedName (Sure ["Franco", "Washignton"]) (Sure ["Tata", "Gonzalo"]))
    it "OS N"  $ run (NameAndSurname ["Washignton", "Tata"] ["Ana"]) `shouldBe` (FixedName (Sure ["Ana"]) (Sure ["Washignton", "Tata"]))

    it "OS N"  $ run (FullName ["Federico", "Scarpa"]) `shouldBe` (FixedName (Sure ["Federico"]) (Sure ["Scarpa"]))
    it "OS N"  $ run (FullName ["Villani", "Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Sure ["Villani"]))
    it "OS N"  $ run (FullName ["Florencia", "Tata", "Finzi"]) `shouldBe` (FixedName (Sure ["Florencia"]) (Sure ["Tata", "Finzi"]))
    it "OS N"  $ run (FullName ["Rodrigo", "Juan", "Alfonso"]) `shouldBe` (FixedName (Sure ["Rodrigo", "Juan"]) (Unsure ["Alfonso"]))

    it "S S" $ run (NameAndSurname ["Scarpa"] ["Tata"]) `shouldBe` (FixedName (Unsure ["Scarpa"]) (Unsure ["Tata"]))

    it "NOOSOO"  $ run (FullName ["Ana", "María", "Beatriz", "Rodríguez", "Alfonso", "De la Cruz"]) `shouldBe` (
                                                                                                        FixedName (Sure ["Ana", "María", "Beatriz"]) (Sure ["Rodríguez", "Alfonso", "De la Cruz"]))


  describe "fixes" $ do
    it "" $  do
      fixes sampleBag (FullName ["Ana", "María", "Beatriz", "Rodríguez", "Alfonso", "De la Cruz"]) `shouldBe` [
        FixedName (Sure ["Ana"]) (Unsure ["María","Beatriz","Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Sure ["Ana","María"]) (Unsure ["Beatriz","Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Sure ["Ana","María","Beatriz"]) (Sure ["Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Unsure ["Ana","María","Beatriz","Rodríguez"]) (Unsure ["Alfonso","De la Cruz"]),
        FixedName (Unsure ["Ana","María","Beatriz","Rodríguez","Alfonso"]) (Unsure ["De la Cruz"])]
