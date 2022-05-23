{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day13
  ( part1,
    part2,
  )
where

import Paths_advent_of_code (getDataFileName)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Control.Monad
import Data.Foldable (foldl')

type Point = (Int, Int)
type Sheet = S.Set Point

data Axis = X | Y
type Fold = (Axis, Int)

part1 :: IO Int
part1 = do
  (points, folds) <- input
  return $ length $ fold points (head folds)

part2 :: IO Int
part2 = do
  (points, folds) <- input
  let s =  foldl' fold points folds
  print "\n"
  printGrid s
  return $ 2

bounds :: Sheet -> Point
bounds s = foldl' (\(x,y) (x',y')-> (max x x', max y y')) (0,0) $ S.elems s

-- https://github.com/snhmibby/advent-of-code-2021/blob/main/13/13.hs
printGrid :: Sheet -> IO ()
printGrid s = do
  let (y,x) = bounds s
  forM_ [0..x] $ \i -> do
    forM_ [0..y] $ \j -> do
      if (j,i) `S.member` s
         then putStr "#"
         else putStr " "
    putStrLn ""

fold :: Sheet -> Fold -> Sheet 
fold s f = S.map (fold' f) s
  where
    fold' (Y, n) (x, y) = if y > n then (x, y - (y - n) * 2) else (x, y)
    fold' (X, n) (x, y) = if x > n then (x - (x - n) * 2, y) else (x, y)

input :: IO (Sheet, [Fold])
input =  do
  [rawPoints, rawFolds] <- map lines . splitOn "\n\n" <$> (readFile =<< getDataFileName "inputs/day13.txt")
  let points =  S.fromList $ map (createPoint . splitOn ",")  rawPoints
  let folds = map createInstruction rawFolds 
  return (points, folds)
  where
    createPoint [x, y] = (read x, read y)
    createInstruction i = case words i of
      [_, _, 'x':'=':xs] -> (X, read xs)
      [_, _, 'y':'=':ys] -> (Y, read ys)
  