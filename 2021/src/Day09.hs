module Day09
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Ord (Down (Down))
import Helpers (enumerate)
import Paths_advent_of_code (getDataFileName)

type Point = (Int, Int)

type Grid = M.Map Point Int

part1 :: IO Int
part1 = do
  grid <- createGrid <$> input
  return $ sum [grid ! p + 1 | p <- M.keys grid, isLowPoint p grid]

part2 :: IO Int
part2 = do
  grid <- createGrid <$> input
  let lowPoints = [p | p <- M.keys grid, isLowPoint p grid]
  let basins = map (\x -> basin grid [] [x]) lowPoints
  return $ product . map length . take 3 $ sortOn (Down . length) basins

basin :: Grid -> [Point] -> [Point] -> [Point]
basin grid visited [] = visited
basin grid visited (x : xs)
  | x `elem` visited = basin grid visited xs
  | isValid grid x = basin grid (x : visited) (neighbors x ++ xs)
  | otherwise = basin grid visited xs

isValid :: Grid -> Point -> Bool
isValid grid point = M.member point grid && ((grid ! point) /= 9)

isLowPoint :: Point -> Grid -> Bool
isLowPoint p grid = do
  all (== True) [grid ! p < (grid ! n) | n <- neighbors p, M.member n grid]

neighbors :: Point -> [Point]
neighbors (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

createGrid :: [[Int]] -> M.Map Point Int
createGrid rawGrid = M.fromList [((x, y), v) | (y, row) <- enumerate rawGrid, (x, v) <- enumerate row]

input :: IO [[Int]]
input = map (map digitToInt) . lines <$> (readFile =<< getDataFileName "inputs/day09.txt")