{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day08
  ( part1,
    part2,
  )
where

import Helpers (readCommaSeparatedInts)
import Paths_advent_of_code (getDataFileName)
import Data.List.Split ( splitOn )
import qualified Data.List as Data.Text
import qualified Data.Map.Strict as M
import Control.Monad (join)
import qualified Data.List as L
import Data.List as L
import Data.List (findIndex)
import Data.Maybe (fromJust)

-- Digit -> Segment
-- 1 -> 2 (cf)
-- 4 -> 4 (bcdf)
-- 7 -> 3 (acf)
-- 8 -> 7 (abcdefg)
part1 :: IO Int
part1 = do
  outputs <- getOutputs
  return $ length [o | o <- outputs, length o `elem` [2, 4, 3, 7]]

part2 :: IO Int
part2 = do
  content <- readFile =<< getDataFileName "inputs/day08.txt"
  return $ sum . map parseLine $ lines content

parseLine :: String -> Int
parseLine line = do
  let tokens = splitOn " | " line
  let signal = words $ head tokens
  let output = words $ last tokens

  let sorted = L.sortBy (\a b -> compare (length a) (length b)) signal
  let [one, four] = [sorted!!0, sorted!!2]
  read [decodeDigit o one four | o <- output] :: Int

decodeDigit :: String -> String -> String -> Char
decodeDigit output one four = case length output of
  2 -> '1'
  4 -> '4'
  3 -> '7'
  7 -> '8'
  5 -> decodeLengthFive output one four
  _ -> decodeLengthSix output one four


-- Digits with length 5: 3 (acdfg), 2 (acdeg) and 5 (abdfg)
-- Check intersection with 1 (cf)
-- Check intersection with 4 (bcdf)
decodeLengthFive :: String -> String -> String -> Char
decodeLengthFive digit one four = do
  if length (digit `intersect` one) == 2
    then '3'
  else if length (digit `intersect` four) == 2
    then '2'
  else
    '5'

-- Digits with length 6: 6 (abdefg), 9 (abcdfg), 0 (abcefg)
-- Check intersection with 1 (cf)
-- Check intersection with 4 (bcdf)
decodeLengthSix :: String -> String -> String -> Char
decodeLengthSix digit one four = do
  if length (digit `intersect` one) == 1
    then '6'
  else if length (digit `intersect` four) == 4
    then '9'
  else
    '0'

getOutputs :: IO [String]
getOutputs = concatMap (words . last . splitOn "|") <$> input

input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day08.txt")