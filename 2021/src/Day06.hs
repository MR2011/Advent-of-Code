module Day06
  ( part1,
    part2,
  )
where

import Helpers (readCommaSeparatedInts)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = do
  l <- initCountList <$> input
  return $ sum $ iterate step l !! 80

part2 :: IO Int
part2 = do
  l <- initCountList <$> input
  return $ sum $ iterate step l !! 256

-- Groups of numbers [0..8]
-- 0 spawns new fish (a moves to the end)
-- 0 becomes 6 (a+h)
step :: [Int] -> [Int]
step [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, a + h, i, a]
step _ = []

-- Numbers: 0-8
-- Instead of managing each fish/number, group them and manage the groups
initCountList :: [Int] -> [Int]
initCountList l = [count i l | i <- [0 .. 8]]

count :: Int -> [Int] -> Int
count v l = length $ filter (== v) l

input :: IO [Int]
input = readCommaSeparatedInts "inputs/day06.txt"