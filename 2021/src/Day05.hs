{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.List.Split.Internals (wordsBy)
import Paths_advent_of_code (getDataFileName)

type Point = (Int, Int)

type Line = (Point, Point)

part1 :: IO Int
part1 = length . filter (>= 2) . map length . group . sort . concatMap flattenLine . filter isHV <$> input

part2 :: IO Int
part2 = length . filter (>= 2) . map length . group . sort . concatMap flattenLine <$> input

isHV :: Line -> Bool
isHV ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

flattenLine :: Line -> [Point]
flattenLine ((x1, y1), (x2, y2)) = [(x1 + i * xStep, y1 + i * yStep) | i <- [0 .. l]]
  where
    dy = y2 - y1
    dx = x2 - x1
    l = max (abs dx) (abs dy)
    xStep = dx `div` l
    yStep = dy `div` l

input :: IO [Line]
input = map (createLine . wordsBy (not . isDigit)) . lines <$> (readFile =<< getDataFileName "inputs/day05.txt")
  where
    createLine [x1, y1, x2, y2] = ((read x1, read y1), (read x2, read y2))
