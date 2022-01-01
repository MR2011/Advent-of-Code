module Day07
  ( part1,
    part2,
  )
where

import Helpers (readCommaSeparatedInts)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = do
  crabs <- input
  return $ minimum [sum [abs (crab - pos) | crab <- crabs] | pos <- [(minimum crabs) .. (maximum crabs)]]

part2 :: IO Int
part2 = do
  crabs <- input
  return $ minimum [sum [gauss (abs (crab - pos)) | crab <- crabs] | pos <- [(minimum crabs) .. (maximum crabs)]]

gauss :: Int -> Int
gauss n = div (n * n + n) 2

input :: IO [Int]
input = readCommaSeparatedInts "inputs/day07.txt"