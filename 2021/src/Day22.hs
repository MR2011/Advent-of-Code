module Day22
  ( part1,
    part2,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Paths_advent_of_code (getDataFileName)
import Text.Parsec
  ( char,
    digit,
    many1,
    newline,
    parse,
    sepEndBy1,
    string,
    try,
  )
import Text.ParserCombinators.Parsec (Parser, (<|>))

type Point = (Int, Int, Int)

data State = On | Off deriving (Show, Eq)

data Instruction = Instruction State (Int, Int) (Int, Int) (Int, Int) deriving (Show, Eq)

part1 :: IO Int
part1 = do
  S.size . foldl evaluateInstruction S.empty <$> input

part2 :: IO Int
part2 = do
  return $ 2

evaluateInstruction :: S.Set Point -> Instruction -> S.Set Point
evaluateInstruction active (Instruction state (xMin, xMax) (yMin, yMax) (zMin, zMax)) =
  case state of
    On -> S.union active newSet
    Off -> S.difference active newSet
  where
    cap n
      | n > 50 = 51
      | n < -50 = -51
      | otherwise = n
    inRange n = n <= 50 && n >= -50
    newSet =
      S.fromList
        [(x, y, z) | x <- [cap xMin .. cap xMax], y <- [cap yMin .. cap yMax], z <- [cap zMin .. cap zMax], all inRange [x, y, z]]

instructionParser :: Parser Instruction
instructionParser = do
  state <- try (On <$ string "on") <|> try (Off <$ string "off")
  string " x="
  xMin <- signedInt
  string ".."
  xMax <- signedInt
  string ",y="
  yMin <- signedInt
  string ".."
  yMax <- signedInt
  string ",z="
  zMin <- signedInt
  string ".."
  zMax <- signedInt
  pure $ Instruction state (xMin, xMax) (yMin, yMax) (zMin, zMax)

signedInt :: Parser Int
signedInt = try posInt <|> negInt
  where
    posInt = do
      s <- many1 digit
      return $ read s
    negInt = do
      char '-'
      n <- posInt
      return (- n)

inputParser :: Parser [Instruction]
inputParser =
  sepEndBy1 instructionParser newline

input :: IO [Instruction]
input = do
  r <- parse inputParser "" <$> (readFile =<< getDataFileName "inputs/day22.txt")
  case r of
    Left err -> do
      print err
      return []
    Right v -> return v