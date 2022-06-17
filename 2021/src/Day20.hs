{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day20
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Paths_advent_of_code (getDataFileName)

type Point = (Int, Int)

type Image = M.Map Point Char

type Algorithm = String

part1 :: IO Int
part1 = do
  (algorithm, img) <- input
  return $ length . M.toList . M.filter (== '#') $ steps 2 img algorithm

part2 :: IO Int
part2 = do
  (algorithm, img) <- input
  return $ length . M.toList . M.filter (== '#') $ steps 50 img algorithm

steps :: Int -> Image -> Algorithm -> Image
steps 0 img a = img
steps n img a = steps (n - 1) nextImg a
  where
    nextImg = step img a (defaultValue n)

step :: Image -> Algorithm -> Char -> Image
step img a def =
  M.mapWithKey
    (\k v -> enhance k a nextImg def)
    nextImg
  where
    nextImg = expandImage img def

defaultValue :: Int -> Char
defaultValue n = if odd n then '#' else '.'

expandImage :: Image -> Char -> Image
expandImage img def = do
  let points = M.keys img
  let xMin = minimum (map fst points) - 1
  let xMax = maximum (map fst points) + 1
  let yMin = minimum (map snd points) - 1
  let yMax = maximum (map snd points) + 1
  let border = [(i, j) | i <- [xMin .. xMax], j <- [yMin .. yMax]]
  M.union img $ M.fromList $ map (\k -> (k, def)) border

neighbors :: Point -> Image -> Char -> [Char]
neighbors (x, y) img def =
  [ (\p -> M.findWithDefault def p img) (x + i, y + j)
    | i <- [- 1 .. 1],
      j <- [- 1 .. 1]
  ]

enhance :: Point -> Algorithm -> Image -> Char -> Char
enhance p a img def = a !! binToDec (neighbors p img def)

binToDec :: String -> Int
binToDec = foldl (\acc n -> toInt n + 2 * acc) 0
  where
    toInt '.' = 0
    toInt '#' = 1

input :: IO (Algorithm, Image)
input = do
  [algorithm, rawGrid] <- splitOn "\n\n" <$> (readFile =<< getDataFileName "inputs/day20.txt")
  let grid = M.fromList [((x, y), point) | (x, row) <- zip [0 ..] (lines rawGrid), (y, point) <- zip [0 ..] row]
  return (algorithm, grid)