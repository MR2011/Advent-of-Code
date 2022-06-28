{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day25
  ( part1,
    part2,
  )
where

import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.SBV.Internals as M
import Helpers (enumerate)
import Paths_advent_of_code (getDataFileName)

type Coord = (Int, Int)

type Boundaries = (Int, Int)

data Grid = Grid Boundaries (M.Map Coord Cucumber)

data Cucumber = East | South deriving (Eq, Show)

part1 :: IO Int
part1 = do
  grid@(Grid _ g) <- input
  return $ length $ simulate [grid]

part2 :: IO Int
part2 = do
  return $ 2

simulate :: [Grid] -> [Grid]
simulate gs@(grid@(Grid _ g) : _) = if notChanged then gs else simulate (grid' : gs)
  where
    grid'@(Grid _ g') = step grid
    notChanged = null (M.difference g g')

step :: Grid -> Grid
step = stepSouth . stepEast

stepEast :: Grid -> Grid
stepEast g = moveCucumbers g East

stepSouth :: Grid -> Grid
stepSouth g = moveCucumbers g South

filterByCucumber :: Grid -> Cucumber -> Grid
filterByCucumber g@(Grid boundaries grid) cucumber = Grid boundaries $ M.filter (== cucumber) grid

moveCucumbers :: Grid -> Cucumber -> Grid
moveCucumbers g@(Grid boundaries grid) cucumber = Grid boundaries $ M.union notMoved $ M.union moved blocked
  where
    (Grid _ toMove) = filterByCucumber g cucumber
    notMoved = M.difference grid toMove
    moved = M.fromList [(nextCoord boundaries k cucumber, cucumber) | k <- M.keys toMove, canMove (Grid boundaries grid) k cucumber]
    blocked = M.fromList [(k, cucumber) | k <- M.keys toMove, not $ canMove (Grid boundaries grid) k cucumber]

canMove :: Grid -> Coord -> Cucumber -> Bool
canMove g@(Grid boundaries grid) coord cucumber = do
  let next = nextCoord boundaries coord cucumber
  M.notMember next grid

nextCoord :: Boundaries -> Coord -> Cucumber -> Coord
nextCoord (xMax, yMax) (x, y) c
  | c == East = ((x + 1) `mod` xMax, y)
  | c == South = (x, (y + 1) `mod` yMax)

createGrid :: [String] -> M.Map Coord Cucumber
createGrid rawGrid = M.fromList [((x, y), parseTile v) | (y, row) <- enumerate rawGrid, (x, v) <- enumerate row, v /= '.']
  where
    parseTile t
      | t == '>' = East
      | t == 'v' = South

input :: IO Grid
input = do
  raw <- lines <$> (readFile =<< getDataFileName "inputs/day25.txt")
  let yMax = length raw
  let xMax = length $ head raw
  let g = createGrid raw
  return $ Grid (xMax, yMax) g
