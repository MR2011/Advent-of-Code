{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day04
  ( part1,
    part2,
  )
where

import Data.List
  ( find,
    transpose,
  )
import Data.List.Split.Internals (chunksOf)
import Helpers
  ( readCommaSeparatedIntsString,
  )
import Paths_advent_of_code (getDataFileName)

type BingoBoard = [[(Int, Bool)]]

part1 :: IO Int
part1 = do
  (drawings, boards) <- input
  let (drawing, winner) = playBingo drawings boards
  case checkSum drawing <$> winner of
    Just score -> return score
    Nothing -> return $ -1

part2 :: IO Int
part2 = do
  (drawings, boards) <- input
  let (drawing, winner) = playBingo' drawings boards
  case checkSum drawing <$> winner of
    Just score -> return score
    Nothing -> return $ -1

markNum :: Int -> BingoBoard -> BingoBoard
markNum num = map (map (\(n, m) -> (n, m || n == num)))

checkWinner :: BingoBoard -> Bool
checkWinner board = any (all snd) (board ++ transpose board)

firstWinner :: [BingoBoard] -> Bool
firstWinner = any checkWinner

allWinner :: [BingoBoard] -> Bool
allWinner = all checkWinner

input :: IO ([Int], [BingoBoard])
input = do
  l <-
    filter (/= []) . lines <$> (readFile =<< getDataFileName "inputs/day04.txt")
  let firstRow = readCommaSeparatedIntsString $ head l
  let bingoBoards = chunksOf 5 $ map (map ((,False) . read) . words) $ tail l
  return (firstRow, bingoBoards)

markAll :: Int -> [BingoBoard] -> [BingoBoard]
markAll num = map (markNum num)

checkSum :: Int -> BingoBoard -> Int
checkSum num board = unmarkedSum * num
  where
    unmarkedSum = sum [n | (n, marked) <- concat board, not marked]

-- part 1
playBingo :: [Int] -> [BingoBoard] -> (Int, Maybe BingoBoard)
playBingo (drawing : drawings) cards = do
  let cards' = markAll drawing cards
  if firstWinner cards'
    then (drawing, find checkWinner cards')
    else playBingo drawings cards'

-- part 2
playBingo' :: [Int] -> [BingoBoard] -> (Int, Maybe BingoBoard)
playBingo' (drawing : drawings) cards = do
  let cards' = markAll drawing cards
  if allWinner cards'
    then (drawing, markNum drawing <$> find (not . checkWinner) cards)
    else playBingo' drawings cards'