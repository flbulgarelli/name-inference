module NameSpec (spec) where

import  Test.Hspec
import  Data.Name

sampleBag = Bag [
    "Daniela", "Alfonso", "Rocío", "Federico", "Juan", "María", "Beatriz", "Florencia", "Ana", "Gonzalo", "Rodrigo", "Franco", "Washignton"] [
    "Scarpa", "Alfonso", "Villani", "Tata", "Rodrigo", "Gonzalo", "Finzi", "Rodríguez", "Washignton"]
run = fix sampleBag

spec :: Spec
spec = do
  describe "fix" $ do
    describe "just two words" $ do
      it "N S" $ run (NameAndSurname ["Federico"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico"]) (Sure ["Scarpa"]))
      it "S N" $ run (NameAndSurname ["Villani"] ["Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Sure ["Villani"]))
      it "O O" $ run (NameAndSurname ["Rodrigo"] ["Alfonso"]) `shouldBe` (FixedName (Unsure ["Rodrigo"]) (Unsure ["Alfonso"]))
      it "O O" $ run (NameAndSurname ["Alfonso"] ["Rodrigo"]) `shouldBe` (FixedName (Unsure ["Alfonso"]) (Unsure ["Rodrigo"]))
      it "N O" $ run (NameAndSurname ["Rocío"] ["Rodrigo"]) `shouldBe` (FixedName (Sure ["Rocío"]) (Unsure ["Rodrigo"]))
      it "O N" $ run (NameAndSurname ["Carlos"] ["Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Unsure ["Carlos"]))
      it "O S" $ run (NameAndSurname ["Rodrigo"] ["Finzi"]) `shouldBe` (FixedName (Unsure ["Rodrigo"]) (Sure ["Finzi"]))
      it "S O" $ run (NameAndSurname ["Finzi"] ["Gonzalo"]) `shouldBe` (FixedName (Unsure ["Gonzalo"]) (Sure ["Finzi"]))

      it "S S" $ run (NameAndSurname ["Scarpa"] ["Tata"]) `shouldBe` (FixedName (Unsure ["Scarpa"]) (Unsure ["Tata"]))

    describe "more than two words" $ do
      it "NO S"  $ run (NameAndSurname ["Federico", "Rodrigo"] ["Scarpa"]) `shouldBe` (FixedName (Sure ["Federico", "Rodrigo"]) (Sure ["Scarpa"]))
      it "NO SS" $ run (NameAndSurname ["Federico", "Rodrigo"] ["Scarpa", "Rodríguez"]) `shouldBe` (FixedName (Sure ["Federico", "Rodrigo"]) (Sure ["Scarpa", "Rodríguez"]))
      it "NO SO" $ run (NameAndSurname ["Franco", "Washignton"] ["Tata", "Gonzalo"]) `shouldBe` (FixedName (Sure ["Franco", "Washignton"]) (Sure ["Tata", "Gonzalo"]))
      it "OS N"  $ run (NameAndSurname ["Washignton", "Tata"] ["Ana"]) `shouldBe` (FixedName (Sure ["Ana"]) (Sure ["Washignton", "Tata"]))

    describe "full name" $ do
      it "OSN"  $ run (FullName ["Federico", "Scarpa"]) `shouldBe` (FixedName (Sure ["Federico"]) (Sure ["Scarpa"]))
      it "OSN"  $ run (FullName ["Villani", "Daniela"]) `shouldBe` (FixedName (Sure ["Daniela"]) (Sure ["Villani"]))
      it "OSN"  $ run (FullName ["Florencia", "Tata", "Finzi"]) `shouldBe` (FixedName (Sure ["Florencia"]) (Sure ["Tata", "Finzi"]))
      it "OSN"  $ run (FullName ["Rodrigo", "Juan", "Alfonso"]) `shouldBe` (FixedName (Sure ["Rodrigo", "Juan"]) (Unsure ["Alfonso"]))

      it "NNNSOO"  $ run (FullName ["Ana", "María", "Beatriz", "Rodríguez", "Alfonso", "De la Cruz"]) `shouldBe` (
                                                                                                          FixedName (Sure ["Ana", "María", "Beatriz"]) (Unsure ["Rodríguez", "Alfonso", "De la Cruz"]))

      it "NNNSOO"  $ run (FullName ["Ana", "María", "Beatriz", "Rodríguez", "De la Cruz", "Alfonso"]) `shouldBe` (
                                                                                                          FixedName (Sure ["Ana", "María", "Beatriz"]) (Sure ["Rodríguez", "De la Cruz", "Alfonso"]))

      it "NONS"  $ run (FullName ["Ana", "Mar", "Beatriz", "Rodríguez"]) `shouldBe` (FixedName (Sure ["Ana", "Mar", "Beatriz"]) (Sure ["Rodríguez"]))
      it "NNOS"  $ run (FullName ["Ana", "Beatriz", "Bea", "Rodríguez"]) `shouldBe` (FixedName (Unsure ["Ana", "Beatriz", "Bea"]) (Sure ["Rodríguez"]))
      it "NNOS"  $ run (FullName ["Rodríguez", "Ana", "Beatriz", "Bea"]) `shouldBe` (FixedName (Unsure ["Ana", "Beatriz", "Bea"]) (Sure ["Rodríguez"]))


  describe "fixes" $ do
    it "" $  do
      fixes sampleBag (FullName ["Ana", "María", "Beatriz", "Rodríguez", "Alfonso", "De la Cruz"]) `shouldBe` [
        FixedName (Sure ["Ana"]) (Unsure ["María","Beatriz","Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Sure ["Ana","María"]) (Unsure ["Beatriz","Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Sure ["Ana","María","Beatriz"]) (Unsure ["Rodríguez","Alfonso","De la Cruz"]),
        FixedName (Unsure ["Ana","María","Beatriz","Rodríguez"]) (Unsure ["Alfonso","De la Cruz"]),
        FixedName (Unsure ["Ana","María","Beatriz","Rodríguez","Alfonso"]) (Unsure ["De la Cruz"])]
