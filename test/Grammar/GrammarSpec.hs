module Grammar.GrammarSpec
  ( spec
  ) where

import Test.Hspec
import Data.Matrix

import Grammar

spec :: Spec
spec = do
  describe "Command" $ do
    it "accesses InitQ fields" $ do
      let x = InitQ "a" (QRef "b")
      qName x `shouldBe` "a"
      qVal x `shouldBe` QRef "b"
    it "accesses InitG fields" $ do
      let x = InitG "a" (GRef "b")
      gName x `shouldBe` "a"
      gVal x `shouldBe` GRef "b"
    it "shows Commands correctly" $ do
      show (InitQ "a" (QRef "b")) `shouldBe` "a := (b: QBit)"
      show (InitG "a" (GRef "b")) `shouldBe` "a := (b: Gate)"
      show (Measure (QRef "b")) `shouldBe` "Measure (b: QBit)"
      show (Return (QRef "b")) `shouldBe` "Return (b: QBit)"
    it "shows QBits correctly" $ do
      show (QRef "a") `shouldBe` "(a: QBit)"
      show (QArr [1, 2, 3]) `shouldBe` "[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]"
      show (App (GRef "a") (QRef "b")) `shouldBe` "(a: Gate) <- (b: QBit)"
    it "shows Gates correctly" $ do
      show (GRef "a") `shouldBe` "(a: Gate)"
      show (GMatrix (identity 10)) `shouldBe` "[[]]"
      show (Tensor (GRef "a") (GRef "b")) `shouldBe` "(a: Gate) * (b: Gate)"
      show (Product (GRef "a") (GRef "b")) `shouldBe` "(a: Gate) . (b: Gate)"
