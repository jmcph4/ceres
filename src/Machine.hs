module Machine where

import           Data.Maybe
import           Data.List

type Stack = [Word]
type Memory = [Word]

data State = State
    { pc     :: Word
    , reg    :: Word
    , stack  :: Stack
    , memory :: Memory
    }
    deriving (Show, Eq)

data Opcode = Add | Sub | Mul | Div | Mod | Jump | Push | Pop | Load | Store | Set | Call | Ret | Pos | Dup | Swap | Halt | Nop
    deriving (Enum, Show, Eq)

data Instruction = Instruction
    { opcode :: Opcode
    , arg    :: Maybe Word
    }
    deriving (Show, Eq)

type Code = [Instruction]

type Program = Code

op2 :: (Word -> Word -> Word) -> Stack -> Stack
op2 f (a : b : xs) = [f a b] ++ xs
op2 f _            = error "Expected two arguments, received one"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs) | n == 0    = newVal : xs
                             | otherwise = x : replaceNth (n - 1) newVal xs

add :: State -> State
add s = s { pc = (pc s) + 1, stack = op2 (+) (stack s) }

sub :: State -> State
sub s = s { pc = (pc s) + 1, stack = op2 (-) (stack s) }

mul :: State -> State
mul s = s { pc = (pc s) + 1, stack = op2 (*) (stack s) }

div :: State -> State
div s = s { pc = (pc s) + 1, stack = op2 (Prelude.div) (stack s) }

mod :: State -> State
mod s = s { pc = (pc s) + 1, stack = op2 (Prelude.mod) (stack s) }

jump :: State -> Word -> State
jump s dest = s { pc = dest }

push :: State -> State
push s = if (length (stack s)) == 1024
    then error "Stack full"
    else s { pc = (pc s) + 1, stack = [reg s] ++ stack s }

pop :: State -> State
pop s = if null (stack s)
    then error "Stack empty"
    else State (pc s + 1) (head (stack s)) (tail (stack s)) (memory s)

load :: State -> State
load s = s
    { pc    = (pc s) + 1
    , stack = replaceNth 0
                         ((memory s) !! (fromIntegral (head (stack s))))
                         (stack s)
    }

store :: State -> State
store s = s
    { pc     = (pc s) + 1
    , stack  = tail (tail (stack s))
    , memory = replaceNth (fromIntegral (head (stack s)))
                          (head (tail (stack s)))
                          (memory s)
    }

set :: State -> Word -> State
set s arg = s { pc = (pc s) + 1, reg = arg }

call :: State -> State
call s = s { pc = (pc s) + 1, stack = [pc s] ++ (stack s) }

ret :: State -> State
ret s = s { pc = head (stack s), stack = tail (stack s) }

pos :: State -> State
pos s = s { pc = (pc s) + 1, stack = [pc s] ++ stack s }

dup :: State -> State
dup s = s { pc = (pc s) + 1, stack = [(head (stack s))] ++ (stack s) }

swap :: State -> State
swap s = s
    { pc    = (pc s) + 1
    , stack = [head (tail (stack s)), head (stack s)] ++ (tail (tail (stack s)))
    }

halt :: State -> State
halt st = error "Halted"

nop :: State -> State
nop s = s { pc = (pc s) + 1 }

step :: State -> Instruction -> Maybe State
step st inst = case (opcode inst) of
    Add   -> Just $ add st
    Sub   -> Just $ sub st
    Mul   -> Just $ mul st
    Div   -> Just $ Machine.div st
    Mod   -> Just $ Machine.mod st
    Jump  -> Just $ jump st (fromJust $ (arg inst))
    Push  -> Just $ push st
    Pop   -> Just $ pop st
    Load  -> Just $ load st
    Store -> Just $ store st
    Set   -> Just $ set st (fromJust $ (arg inst))
    Call  -> Just $ call st
    Ret   -> Just $ ret st
    Pos   -> Just $ pos st
    Swap  -> Just $ swap st
    Dup   -> Just $ dup st
    Halt  -> Nothing
    Nop   -> Just $ nop st

run :: State -> Program -> State
run ss prog = case step ss (prog !! fromIntegral (pc ss)) of
    Nothing  -> ss
    Just ss' -> run ss' prog

startState :: State
startState = State 0 0 [] (replicate 16 0)

runMachine :: Program -> State
runMachine prog = run startState prog

