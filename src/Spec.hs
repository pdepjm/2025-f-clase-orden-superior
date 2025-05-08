module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
    it "Un jedi esta preparado si su potencial de combate es mayor a 5000 y tiene experiencia" $ do
      estaPreparado yoda `shouldBe` True
    it "Un jedi no esta preparado si no tiene experiencia" $ do
      estaPreparado anakin `shouldBe` False


