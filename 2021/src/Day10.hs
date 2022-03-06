{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day10
  ( part1,
    part2,
  )
where

import Paths_advent_of_code (getDataFileName)
import Data.Maybe (mapMaybe)
import Data.List (sort)

validateCorrupted :: [Char] -> [Char] -> Maybe Char
validateCorrupted _ [] = Nothing
validateCorrupted stack (c:cs)
    | c `elem` ['(', '[', '{', '<'] = validateCorrupted (closing c : stack) cs
    | c == head stack = validateCorrupted (tail stack) cs
    | otherwise = Just c

validateIncomplete :: [Char] -> [Char] -> Maybe [Char]
validateIncomplete [] [] = Nothing
validateIncomplete stack [] = Just stack
validateIncomplete stack (c:cs)
    | c `elem` ['(', '[', '{', '<'] = validateIncomplete (closing c : stack) cs
    | c == head stack = validateIncomplete (tail stack) cs
    | otherwise = Nothing

part1 :: IO Int
part1 = do
    chunks <- lines <$> input
    return $ sum . map errorScore $ mapMaybe (validateCorrupted []) chunks

part2 :: IO Int
part2 = do
    chunks <- lines <$> input
    return . middle . sort $ map (foldl (\acc x -> completionScore x + (acc * 5)) 0) $ mapMaybe (validateIncomplete []) chunks
    where
      middle l = l !! (length l `div` 2)

closing :: Char -> Char
closing c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

errorScore :: Char -> Int
errorScore c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

completionScore :: Char -> Int
completionScore c = case c of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4

input :: IO String
input = readFile =<< getDataFileName "inputs/day10.txt"