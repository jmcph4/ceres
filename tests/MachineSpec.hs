module MachineSpec where

import Test.Hspec

import Machine

main :: IO ()
main = hspec $ do
    describe "nop" $ do
        it "Only modifies program counter" $
            nop startState `shouldBe` (startState { pc = 1 })

