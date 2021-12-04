{-# LANGUAGE ViewPatterns #-}

module Day02 (part1, part2) where

import Data.List.Split (splitOn)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = do
  (x, y) <- foldl move (0, 0) <$> input
  return $ x * y
  where
    move :: (Int, Int) -> (String, Int) -> (Int, Int)
    move (x, y) (cmd, n)
      | cmd == "forward" = (x + n, y)
      | cmd == "up" = (x, y - n)
      | cmd == "down" = (x, y + n)
      | otherwise = (x, y)

part2 :: IO Int
part2 = do
  (x, y, _) <- foldl move (0, 0, 0) <$> input
  return $ x * y
  where
    move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
    move (x, y, z) (cmd, n)
      | cmd == "forward" = (x + n, y + (z * n), z)
      | cmd == "up" = (x, y, z - n)
      | cmd == "down" = (x, y, z + n)
      | otherwise = (x, y, z)

input :: IO [(String, Int)]
input = map parse . lines <$> (readFile =<< getDataFileName "inputs/day02.txt")

parse :: String -> (String, Int)
parse line = (cmd, value)
  where
    [cmd, read -> value] = splitOn " " line
