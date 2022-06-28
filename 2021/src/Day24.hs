{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24
  ( part1,
    part2,
  )
where

import Data.SBV
import Paths_advent_of_code (getDataFileName)

type Data = SInt64

data Register = W | X | Y | Z deriving (Eq, Ord, Show, Enum)

data Op = Add | Mul | Div | Mod | Eql deriving (Eq, Show)

data Argument = Reg Register | Lit Integer
  deriving (Eq, Ord, Show)

data Instruction = Inp Register | Instruction Op Register Argument
  deriving (Show)

data State = State {x :: Data, y :: Data, z :: Data, w :: Data} deriving (Show)

-- Source https://topaz.github.io/paste/#XQAAAQB1CQAAAAAAAAA0m0pnuFI8c3wwear+nXxFz1p9ytLVknnSteGtDv8Jxku72LcV1wF+paUbbB0Fo/XE4naGyPe7V9ckytPzSTHlgPJWBKnU0PGiRd7zcyQgf/b+79WVI0yLAtLWS/UX8mGqQqpfUTSsSavBDymR6aNknm4byu74tCtZhogLo9dG+s/kq/4ca0rgAoQ9THJJVPMjHNn2q5i0vha3+/SViJ1Qku3OoOoCriVryqZyw4XOujrgzxamLAYJ9ZZfEpNuXRxvO+U18prY2v+xRk6qCS2/mh23SyLvWlDz+IJvCMN/edBJt7p9HrOW9CjQ0gBMtJKPPqoaVp8cWrCyv8ux5M5yrO7NH+644jvypoWXObSzjR6W0mU64aKgOEumf5t2l76e0VAG0qAeAYtqF2ggFI9VJN68MvElgiiahtOo4PbFMmMxwrxKWQzD+C+e8jFhEZNP4lIx08RVfwGJzenOMjvwPF9kC+jjcCHfUxlVuh/ZvHGOCsVhQQbMNP/pSlY+Jg6ZvgN7nUUblYnrklUimGrcwpPRoEKjO5uamRomZbhRoO3TE/t3ZQHMWjNPjjTkVHJS87QzJsT9YAhru5Z1rw4U+R3z+kmqtG7H7AmdSPs+Wwd9DeiMxkIaN6SOUHskmUn3LaFZvshUOMcmTs7fIGa57Nn3pILR9sytnXhJJnmHjJDQA6el2h0N22T1Ck/lfVqnfkrU3DvnoMOl1ikNIJbQdBlQG9SvflFWu6udquOuxkZveinApIge60Wef8heVGTh20e4ZNxf+xcYX/MweQyNfdK0F3OEwyGAGStADuZOQVR123BaFCv6Mjwru5XN8Lk+wjhVOm2kAGw/En7LPk2znVbFEUsDt5E93+4KwyKpfkey+jaj3ULiw8EWYqOKE8cKyZiCC1rY36bT+GJa98JAm8B0+FCNF/pw77nN3/BjtXyqU6EvVa5vNzdy1p2+Ak9TUo/6TIS/BhLnpSYSm0O7644bztXSNn3y16sTfESm8HXd0zYecf/b9wL+RuE1ZfLOOW4ev2CeDP01zBtM3Kmpbs4spE3CQmStRhQbtZYitbh44Lfoohldmzj0ge7IWu6oxzXCNXyUJZ72iK9P/CemXPPLekpXC0ui3xcbBDCHocNr9O+mNcAigp08ZTd0hIHCEmfEaKSCbSV6+8lpx3vCIzSEHrUw+OaMnlwbWMOcHo4eDuBYt5e6l4BGB/FX8ct+U1eUCQ2vN+olakoENC/d64P02SsxcV+uMdb42W+7XyfF/+vi40s=
part1 :: IO Int
part1 = do
  instructions <- input
  result <- largestSolution instructions
  print result
  return 1

part2 :: IO Int
part2 = do
  instructions <- input
  result <- smallestSolution instructions
  print result
  return $ 2

toNum :: [Data] -> Data
toNum = foldl1 (\acc e -> acc * 10 + e)

isValid :: [Data] -> [Instruction] -> SBool
isValid input instructions = validDigits .&& validMonad
  where
    validDigits = sAll (\n -> n .>= 1 .&& n .<= 9) input
    validMonad = z (run input instructions) .== 0

largestSolution :: [Instruction] -> IO OptimizeResult
largestSolution instructions = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid numbers instructions
  maximize "value" $ toNum numbers

smallestSolution :: [Instruction] -> IO OptimizeResult
smallestSolution instructions = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid numbers instructions
  minimize "value" $ toNum numbers

run :: [Data] -> [Instruction] -> State
run = eval (State 0 0 0 0)
  where
    eval state _ [] = state
    eval state (i : is) (Inp reg : instructions) = eval (writeRegister state reg i) is instructions
    eval state inputs (Instruction op dest src : instructions) =
      eval
        ( writeRegister
            state
            dest
            $ evalOp
              op
              (readRegister dest state)
              case src of
                Reg reg' -> readRegister reg' state
                Lit val -> fromIntegral val
        )
        inputs
        instructions
    eval _ _ _ = undefined

evalOp :: Op -> Data -> Data -> Data
evalOp Add = (+)
evalOp Mul = (*)
evalOp Div = sDiv
evalOp Mod = sMod
evalOp Eql = \a b -> ite (a .== b) 1 0

readRegister :: Register -> State -> Data
readRegister X = x
readRegister Y = y
readRegister Z = z
readRegister W = w

writeRegister :: State -> Register -> Data -> State
writeRegister state X v = state {x = v}
writeRegister state Y v = state {y = v}
writeRegister state Z v = state {z = v}
writeRegister state W v = state {w = v}

parseInstruction :: String -> Instruction
parseInstruction ('i' : 'n' : 'p' : ' ' : r) = Inp (parseRegister r)
parseInstruction ('a' : 'd' : 'd' : ' ' : r : ' ' : a) = Instruction Add (parseRegister [r]) (parseArgument a)
parseInstruction ('m' : 'u' : 'l' : ' ' : r : ' ' : a) = Instruction Mul (parseRegister [r]) (parseArgument a)
parseInstruction ('d' : 'i' : 'v' : ' ' : r : ' ' : a) = Instruction Div (parseRegister [r]) (parseArgument a)
parseInstruction ('m' : 'o' : 'd' : ' ' : r : ' ' : a) = Instruction Mod (parseRegister [r]) (parseArgument a)
parseInstruction ('e' : 'q' : 'l' : ' ' : r : ' ' : a) = Instruction Eql (parseRegister [r]) (parseArgument a)

parseRegister :: String -> Register
parseRegister "w" = W
parseRegister "x" = X
parseRegister "y" = Y
parseRegister "z" = Z

parseArgument :: String -> Argument
parseArgument a
  | a `elem` ["w", "x", "y", "z"] = Reg (parseRegister a)
  | '-' `elem` a = Lit ((-1) * read (tail a))
  | otherwise = Lit (read a)

input :: IO [Instruction]
input = map parseInstruction . lines <$> (readFile =<< getDataFileName "inputs/day24.txt")