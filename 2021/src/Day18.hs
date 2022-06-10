{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day18
  ( part1,
    part2,
  )
where

import Control.Applicative
import Data.List (foldl1')
import Data.Maybe (fromJust)
import Paths_advent_of_code (getDataFileName)
import Text.Parsec (char, digit, many1, parse, sepEndBy1, spaces)
import Text.ParserCombinators.Parsec (Parser, (<|>))

data Tree = Pair Tree Tree | Leaf Int
  deriving (Eq)

data Cxt = Top | L Cxt Tree | R Tree Cxt
  deriving (Eq)

type Loc = (Tree, Cxt)

-- Solution from: https://work.njae.me.uk/2021/12/21/advent-of-code-2021-day-18/
part1 :: IO Int
part1 = do
  i <- input
  let total = foldl1' snailAdd i
  return $ magnitude total

part2 :: IO Int
part2 = do
  i <- input
  return $ maximum [magnitude $ snailAdd a b | a <- i, b <- i]

left :: Loc -> Loc
left (Pair l r, c) = (l, L c r)

right :: Loc -> Loc
right (Pair l r, c) = (r, R l c)

top :: Tree -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, Top) = (t, Top)
up (t, L c r) = (Pair t r, c)
up (t, R l c) = (Pair l t, c)

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc -> (Tree -> Tree) -> Loc
modify (t, c) f = (f t, c)

-- Is there a leftmost element we can split?
splittable :: Tree -> Maybe Loc
splittable t = splittableC (top t)

splittableC :: Loc -> Maybe Loc
splittableC t@(Leaf n, _)
  | n >= 10 = Just t -- If number >= 10, the leftmost regular number splits.
  | otherwise = Nothing
splittableC t@(Pair _ _, _) = splittableC (left t) Control.Applicative.<|> splittableC (right t)

split :: Tree -> Maybe Tree
split num =
  case mn0 of
    Nothing -> Nothing
    Just _ -> Just num1
  where
    mn0 = splittable num
    n0 = fromJust mn0
    ((Leaf sn), _) = n0
    ln = sn `div` 2 -- left element of the pair: number divided by two and rounded down
    rn = ln + sn `mod` 2 -- right element of the pair: number divided by two and rounded up
    n1 = modify n0 (\_ -> Pair (Leaf ln) (Leaf rn)) -- create modified tree with the new pair
    (num1, _) = upmost n1 -- get root of modified tree

-- Counter n is used to track the layer
pairAtDepth :: Int -> Tree -> Maybe Loc
pairAtDepth n t = pairAtDepthC n (top t)

-- Count backwards from n to 0
pairAtDepthC :: Int -> Loc -> Maybe Loc
pairAtDepthC _ (Leaf _, _) = Nothing
pairAtDepthC 0 t@(Pair _ _, _) = Just t
pairAtDepthC n t@(Pair _ _, _) = pairAtDepthC (n - 1) (left t) Control.Applicative.<|> pairAtDepthC (n - 1) (right t)

-- Given a pair that explodes, we need to find the rightmost leaf that's to the left of this pair
rightmostOnLeft :: Loc -> Maybe Loc
rightmostOnLeft (_, Top) = Nothing
rightmostOnLeft t@(_, L c r) = rightmostOnLeft $ up t -- going back up the tree where we took the right branch at a pair
rightmostOnLeft t@(_, R l c) = Just $ rightmostNum $ left $ up t -- then take the left branch of that pair and follow it down

rightmostNum :: Loc -> Loc
rightmostNum t@(Leaf _, _) = t
rightmostNum t@(Pair _ _, _) = rightmostNum $ right t

-- Same as righmostOnLeft, but with left and right swapped.
leftmostOnRight :: Loc -> Maybe Loc
leftmostOnRight (_, Top) = Nothing
leftmostOnRight t@(_, R l c) = leftmostOnRight $ up t
leftmostOnRight t@(_, L c r) = Just $ leftmostNum $ right $ up t

leftmostNum :: Loc -> Loc
leftmostNum t@(Leaf _, _) = t
leftmostNum t@(Pair _ _, _) = leftmostNum $ left t

explode :: Tree -> Maybe Tree
explode num =
  case mp0 of
    Nothing -> Nothing
    Just _ -> Just num1
  where
    mp0 = pairAtDepth 4 num
    p0 = fromJust mp0
    ((Pair (Leaf nl) (Leaf nr)), _) = p0
    p1 = case rightmostOnLeft p0 of
      Nothing -> p0
      Just leftReg -> modify leftReg (\(Leaf n) -> Leaf (n + nl))
    p2 = case pairAtDepthC 4 (upmost p1) >>= leftmostOnRight of
      Nothing -> p1
      Just rightReg -> modify rightReg (\(Leaf n) -> Leaf (n + nr))
    p3 = case pairAtDepthC 4 (upmost p2) of
      Nothing -> p2
      Just centrePair -> modify centrePair (\_ -> Leaf 0)
    (num1, _) = upmost p3

-- reduce is the repeated application of explode and split
reduce :: Tree -> Tree
reduce num = maybe num reduce (explode num Control.Applicative.<|> split num)

snailAdd :: Tree -> Tree -> Tree
snailAdd a b = reduce $ Pair a b

magnitude :: Tree -> Int
magnitude (Leaf n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

treeParser :: Parser Tree
treeParser =
  let pairParser = do
        char '['
        left <- treeParser
        char ','
        right <- treeParser
        char ']'
        pure $ Pair left right
   in let numberParser = Leaf . read <$> many1 digit
       in pairParser Text.ParserCombinators.Parsec.<|> numberParser

inputParser :: Parser [Tree]
inputParser =
  sepEndBy1 treeParser spaces

input :: IO [Tree]
input = do
  r <- parse inputParser "" <$> (readFile =<< getDataFileName "inputs/day18.txt")
  case r of
    Left pe -> return []
    Right tr -> return tr
