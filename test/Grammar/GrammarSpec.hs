module Grammar.GrammarSpec
  ( spec
  ) where

import Test.Hspec

import Grammar

spec :: Spec
spec = do
  describe "Command" $ do
    it "accesses InitG fields" $ do
      let x = InitQ "a" (QRef "b")
      qName x `shouldBe` "a"
