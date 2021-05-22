import Text.Printf
import Test.QuickCheck

import Machine

instance Arbitrary State where
    arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_AddIncrementsProgramCounter :: State -> Bool
prop_AddIncrementsProgramCounter st = (pc $ add st) == ((pc st) + 1)

prop_SubIncrementsProgramCounter :: State -> Bool
prop_SubIncrementsProgramCounter st = (pc $ sub st) == ((pc st) + 1)

prop_MulIncrementsProgramCounter :: State -> Bool
prop_MulIncrementsProgramCounter st = (pc $ mul st) == ((pc st) + 1)

prop_DivIncrementsProgramCounter :: State -> Bool
prop_DivIncrementsProgramCounter st = (pc $ Machine.div st) == ((pc st) + 1)

prop_ModIncrementsProgramCounter :: State -> Bool
prop_ModIncrementsProgramCounter st = (pc $ Machine.mod st) == ((pc st) + 1)

prop_PushIncrementsProgramCounter :: State -> Bool
prop_PushIncrementsProgramCounter st = (pc $ push st) == ((pc st) + 1)

prop_PopIncrementsProgramCounter :: State -> Bool
prop_PopIncrementsProgramCounter st = if (length $ stack st) > 0 then (pc $ pop st) == ((pc st) + 1) else True

prop_LoadIncrementsProgramCounter :: State -> Bool
prop_LoadIncrementsProgramCounter st = (pc $ load st) == ((pc st) + 1)

prop_StoreIncrementsProgramCounter :: State -> Bool
prop_StoreIncrementsProgramCounter st = (pc $ store st) == ((pc st) + 1)

prop_CallIncrementsProgramCounter :: State -> Bool
prop_CallIncrementsProgramCounter st = (pc $ call st) == ((pc st) + 1)

prop_PosIncrementsProgramCounter :: State -> Bool
prop_PosIncrementsProgramCounter st = (pc $ pos st) == ((pc st) + 1)

prop_DupIncrementsProgramCounter :: State -> Bool
prop_DupIncrementsProgramCounter st = (pc $ dup st) == ((pc st) + 1)

prop_SwapIncrementsProgramCounter :: State -> Bool
prop_SwapIncrementsProgramCounter st = (pc $ swap st) == ((pc st) + 1)

prop_NopIncrementsProgramCounter :: State -> Bool
prop_NopIncrementsProgramCounter st = (pc $ nop st) == ((pc st) + 1)

tests :: [(String, IO ())]
tests = [
    ("ADD increments program counter", quickCheck prop_AddIncrementsProgramCounter),
    ("SUB increments program counter", quickCheck prop_SubIncrementsProgramCounter),
    ("MUL increments program counter", quickCheck prop_MulIncrementsProgramCounter),
    ("DIV increments program counter", quickCheck prop_DivIncrementsProgramCounter),
    ("MOD increments program counter", quickCheck prop_ModIncrementsProgramCounter),
    ("PUSH increments program counter", quickCheck prop_PushIncrementsProgramCounter),
    ("POP increments program counter", quickCheck prop_PopIncrementsProgramCounter),
    ("LOAD increments program counter", quickCheck prop_LoadIncrementsProgramCounter),
    ("STORE increments program counter", quickCheck prop_StoreIncrementsProgramCounter),
    ("CALL increments program counter", quickCheck prop_CallIncrementsProgramCounter),
    ("POS increments program counter", quickCheck prop_PosIncrementsProgramCounter),
    ("DUP increments program counter", quickCheck prop_DupIncrementsProgramCounter),
    ("SWAP increments program counter", quickCheck prop_SwapIncrementsProgramCounter),
    ("NOP increments program counter", quickCheck prop_NopIncrementsProgramCounter)]

main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

