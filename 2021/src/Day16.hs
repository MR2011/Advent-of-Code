{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day16
  ( part1,
    part2,
  )
where

import Control.Arrow
import Data.Char (digitToInt)
import Paths_advent_of_code (getDataFileName)
import Text.ParserCombinators.Parsec

type Version = Int

type Type = Int

data Packet = Literal Version Type | Operator Version Type [Packet] deriving (Show)

part1 :: IO Int
part1 = do
  s <- input
  return $ case parse parsePacket "" s of
    Right packet -> sumVersions packet
    Left err -> 0

part2 :: IO Int
part2 = do
  s <- input
  return $ case parse parsePacket "" s of
    Right packet -> evaluatePacket packet
    Left err -> 0

parsePacket :: Parser Packet
parsePacket = try parseLiteral <|> parseOperator

parseLiteral :: Parser Packet
parseLiteral = do
  version <- binToDec <$> count 3 digit
  typeID <- binToDec <$> string "100"
  num <- parseLiteralNum
  return . Literal version $ binToDec num

parseLiteralNum :: Parser String
parseLiteralNum = do
  header <- digit
  parseLiteralGroup header

parseLiteralGroup :: Char -> Parser String
parseLiteralGroup '0' = count 4 digit
parseLiteralGroup '1' = do
  prefix <- count 4 digit
  postfix <- parseLiteralNum
  return $ prefix ++ postfix

parseOperator :: Parser Packet
parseOperator = do
  version <- binToDec <$> count 3 digit
  typeId <- binToDec <$> count 3 digit
  lengthTypeID <- binToDec <$> count 1 digit
  parseWithLengthType lengthTypeID version typeId

parseWithLengthType :: Int -> Version -> Type -> Parser Packet
parseWithLengthType 0 version typeID = do
  length <- binToDec <$> count 15 digit
  subPackets <- count length digit
  case parse (many1 parsePacket) "" subPackets of
    Right packets -> return $ Operator version typeID packets
    Left err -> error (show err)
parseWithLengthType 1 version typeID = do
  numPackets <- binToDec <$> count 11 digit
  subPackets <- count numPackets parsePacket
  return $ Operator version typeID subPackets

sumVersions :: Packet -> Int
sumVersions (Literal v n) = v
sumVersions (Operator v _ packets) = v + sum (map sumVersions packets)

evaluatePacket :: Packet -> Int
evaluatePacket (Literal _ n) = n
evaluatePacket (Operator _ 0 p) = sum $ map evaluatePacket p
evaluatePacket (Operator _ 1 p) = product $ map evaluatePacket p
evaluatePacket (Operator _ 2 p) = minimum $ map evaluatePacket p
evaluatePacket (Operator _ 3 p) = maximum $ map evaluatePacket p
evaluatePacket (Operator _ 5 p) = let [l, r] = map evaluatePacket p in fromEnum $ l > r
evaluatePacket (Operator _ 6 p) = let [l, r] = map evaluatePacket p in fromEnum $ l < r
evaluatePacket (Operator _ 7 p) = let [l, r] = map evaluatePacket p in fromEnum $ l == r

hexToBin :: Char -> String
hexToBin c = case c of
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"

binToDec :: String -> Int
binToDec = foldl (\acc n -> digitToInt n + 2 * acc) 0

input :: IO [Char]
input = concatMap hexToBin <$> (readFile =<< getDataFileName "inputs/day16.txt")
