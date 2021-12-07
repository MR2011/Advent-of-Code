module Helpers
  ( readInts,
    readCommaSeparatedInts,
    mostCommon,
    leastCommon,
    invertBinary,
    stringToDec,
  )
where

import Data.Char (digitToInt)
import Data.List (foldl', group, maximumBy, minimumBy, sort)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Paths_advent_of_code

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines <$> (readFile =<< getDataFileName filePath)

-- readCommaSeparatedInts :: FilePath -> IO [Int]
-- readCommaSeparatedInts filePath =
--   map read . splitOn "," <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = last . maximumBy (comparing length) . group . sort


invertBinary :: String -> String
invertBinary = map (\c -> "10" !! read [c])

stringToDec :: String -> Int
stringToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0