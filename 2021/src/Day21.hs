{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21
  ( part1,
    part2,
  )
where

-- Source: https://github.com/bereal/AdventOfCodeHaskell/blob/main/src/Year2021/Day21.hs
import Control.Arrow (first)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import Paths_advent_of_code (getDataFileName)

type Score = Int

type Round = Int

type Position = Int

type Player = (Position, Score)

type Dice = [Int]

type Game = (Player, Player, Round)

type Cache = M.Map Game (Int, Int)

part1 :: IO Int
part1 = do
  game <- input
  return $ fromJust . head . dropWhile isNothing . map (detScore . fst) . iterate playRound . (,detDice) $ game

part2 :: IO Int
part2 = do
  game <- input
  return $ uncurry max . snd . playDirac M.empty $ game

choosePlayer :: Game -> Int -> Game
choosePlayer (p1, p2, round) roll =
  if even round
    then let p1' = movePlayer roll p1 in (p1', p2, round + 1)
    else let p2' = movePlayer roll p2 in (p1, p2', round + 1)

movePlayer :: Int -> Player -> Player
movePlayer roll (pos, score) =
  let pos' = (pos + roll - 1) `mod` 10 + 1
   in (pos', score + pos')

detScore :: Game -> Maybe Int
detScore ((_, s1), (_, s2), round)
  | s1 >= 1000 = Just $ 3 * s2 * round
  | s2 >= 1000 = Just $ 3 * s1 * round
  | otherwise = Nothing

playRound :: (Game, Dice) -> (Game, Dice)
playRound (game, a : b : c : ds) =
  let game' = choosePlayer game (a + b + c)
   in (game', ds)

playRoundDirac :: Game -> Int -> Game
playRoundDirac (p1, p2, 0) roll = let p1' = movePlayer roll p1 in (p1', p2, 1)
playRoundDirac (p1, p2, 1) roll = let p2' = movePlayer roll p2 in (p1, p2', 0)

-- (3 Dice Sum, Frequence of this sum over all permutations)
diracSumFreqs :: [(Int, Int)]
diracSumFreqs = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

detDice :: [Int]
detDice = cycle [1 .. 100]

playDirac :: Cache -> Game -> (Cache, (Int, Int))
playDirac cache game@(player1@(p1, s1), player2@(p2, s2), turn)
  | s2 >= 21 = (cache, (0, 1))
  | s1 >= 21 = (cache, (1, 0))
  | otherwise = case cache !? game of
    Just v -> (cache, v)
    _ ->
      let subGames = map (first (playRoundDirac game)) diracSumFreqs
          update (cache, (wins1, wins2)) (sg, freq) =
            let (cache', (wins1', wins2')) = playDirac cache sg in (cache', (wins1 + freq * wins1', wins2 + freq * wins2'))
          (cache', result) = foldl update (cache, (0, 0)) subGames
       in (M.insert game result cache', result)

input :: IO Game
input = do
  l <- lines <$> (readFile =<< getDataFileName "inputs/day21.txt")
  let [p1, p2] = [(parsePosition p, 0) | p <- l]
  return (p1, p2, 0)
  where
    parsePosition s = read $ last $ words s