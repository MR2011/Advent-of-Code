module Day01 (part1, part2) where

import Helpers (readInts)
import Paths_advent_of_code ()

countInc :: [Int] -> Int
countInc xs = length . filter (== True) $ zipWith (<) xs (tail xs)

part1 :: IO Int
part1 = do
  entries <- readInts "inputs/day01.txt"
  return $ countInc entries

part2 :: IO Int
part2 = do
  entries <- readInts "inputs/day01.txt"
  let sums = zipWith3 (\a b c -> a + b + c) entries (drop 1 entries) (drop 2 entries)
  return $ countInc sums
