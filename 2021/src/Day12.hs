module Day12
  ( part1,
    part2,
  )
where

import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M
import Paths_advent_of_code (getDataFileName)

type Node = String
type Graph = M.Map Node [Node]
type Edge = (Node, Node)
type Path = [Node]
type Visited = [Node]

part1 :: IO Int
part1 = do
  i <- parseInput
  let g = createGraph i
  return $ length $ allPaths g ["start"] [] [] False

part2 :: IO Int
part2 = do
  i <- parseInput
  let g = createGraph i
  return $ length $ allPaths g ["start"] [] [] True

allPaths :: Graph -> [Node] -> Path -> Visited -> Bool -> [Path]
allPaths _ [] _ _ _ = []
allPaths g (n : ns) path visited twice
  | n == "end" = walk ++ [path ++ ["end"]]
  | n `elem` visited = walk ++ if twice then allPaths g (g ! n) (path ++ [n]) visited False else []
  | isLower (n !! 0) = walk ++ allPaths g (g ! n) (path ++ [n]) (n : visited) twice
  | otherwise = walk ++ allPaths g (g ! n) (path ++ [n]) visited twice
  where
    walk = allPaths g ns path visited twice

parseInput :: IO [Edge]
parseInput = filter (\(_, b) -> b /= "start") . concatMap (\[a, b] -> [(a, b), (b, a)]) <$> input

createGraph :: [Edge] -> Graph
createGraph = foldr addEdge M.empty
  where
    addEdge (a, b) = M.insertWith (++) a [b]

input :: IO [[String]]
input = map (splitOn "-") . lines <$> (readFile =<< getDataFileName "inputs/day12.txt")