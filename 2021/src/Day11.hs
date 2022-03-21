module Day11
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import qualified Data.Map as M
import Helpers (enumerate)
import Paths_advent_of_code (getDataFileName)

-- Source https://www.reddit.com/r/haskell/comments/rds41s/advent_of_code_2021_day_11/

type Point = (Int, Int)

type Grid = M.Map Point Int

part1 :: IO Int
part1 = do
  grid <- createGrid <$> input
  return $ snd (simulate grid 100)

part2 :: IO Int
part2 = do
  grid <- createGrid <$> input
  return $ fst $ until allFlashed enumSteps (0, (grid, 0))
  where
    allFlashed (_, (_, flashes)) = flashes == 100
    enumSteps (steps, (grid, _)) = (steps + 1, step grid)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

applyFlash :: Grid -> (Grid, Int)
applyFlash grid = length <$> applyFlash' (grid, [])

applyFlash' :: (Grid, [Point]) -> (Grid, [Point])
applyFlash' (grid, flashed) = do
  let toFlash = M.keys $ M.filter (> 9) grid
  let allToFlash = concatMap neighbors toFlash
  let flashedGrid = foldr (M.adjust (+ 1)) grid allToFlash
  let flashedGrid' = foldr (M.adjust (const 0)) flashedGrid (flashed ++ toFlash)
  -- No flashes -> complete
  if null toFlash
    then (flashedGrid', flashed)
    else -- flashes -> check for neighbors
      applyFlash' (flashedGrid', flashed ++ toFlash)

step :: Grid -> (Grid, Int)
step = applyFlash . M.map (+ 1)

simulate :: Grid -> Int -> (Grid, Int)
simulate grid n = foldr sumStep (grid, 0) [1 .. n]
  where
    sumStep :: Int -> (Grid, Int) -> (Grid, Int)
    sumStep _ (grid, acc) = (+) acc <$> step grid

createGrid :: [[Int]] -> M.Map Point Int
createGrid rawGrid = M.fromList [((x, y), v) | (y, row) <- enumerate rawGrid, (x, v) <- enumerate row]

input :: IO [[Int]]
input = map (map digitToInt) . lines <$> (readFile =<< getDataFileName "inputs/day11.txt")