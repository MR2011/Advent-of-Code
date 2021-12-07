module Day03 (part1, part2) where

import Data.List (transpose)
import Helpers (invertBinary, mostCommon, stringToDec)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = do
  gamma <- map mostCommon . transpose <$> input
  let epsilon = invertBinary gamma
  return $ stringToDec gamma * stringToDec epsilon

part2 :: IO Int
part2 = do
  report <- input
  let oxygen = rate (>=) 0 report
  let co2 = rate (<) 0 report
  return $ stringToDec oxygen * stringToDec co2

rate :: (Int -> Int -> Bool) -> Int -> [String] -> String
rate op i xs =
  if length xs > 1
    then rate op (i + 1) $ filter (\x -> x !! i == map (criteria op) (transpose xs) !! i) xs
    else head xs

criteria :: (Int -> Int -> Bool) -> String -> Char
criteria op s = if op (countChar s '1') (countChar s '0') then '1' else '0'

countChar :: String -> Char -> Int
countChar s c = length $ filter (== c) s

input :: IO [[Char]]
input = lines <$> (readFile =<< getDataFileName "inputs/day03.txt")
