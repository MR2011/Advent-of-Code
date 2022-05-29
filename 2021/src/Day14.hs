module Day14
  ( part1,
    part2,
  )
where

import Paths_advent_of_code (getDataFileName)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Map ((!))

type Template = String
type Rules = M.Map String [String]
type Pairs = M.Map String Int

part1 :: IO Int
part1 = do
  (template, rules) <- input
  let pairs = parsePairs template rules
  let polymer = iterate (step rules) pairs !! 10
  let frequencies = countCharFequencies polymer
  return $ computeScore frequencies

part2 :: IO Int
part2 = do
  (template, rules) <- input
  let pairs = parsePairs template rules
  let polymer = iterate (step rules) pairs !! 40
  let frequencies = countCharFequencies polymer
  return $ computeScore frequencies
  

step :: Rules -> Pairs-> Pairs
step rules pairs = foldl (M.unionWith (+)) M.empty $ map (uncurry M.singleton) $ step' rules $ M.toList pairs

step' :: Rules -> [(String, Int)] -> [(String, Int)]
step' rules pairs = do
  (pair, count) <- pairs
  nextPair <- rules ! pair
  [(nextPair, count)]

parsePairs :: Template -> Rules -> Pairs
parsePairs template rules = M.fromList [(pair, count template pair ) | pair <- M.keys rules]
  where
    count s x = length (splitOn x s) - 1

computeScore :: [Int] -> Int
computeScore freqs =  maximum freqs - minimum freqs - 1

countCharFequencies :: Pairs -> [Int]
countCharFequencies polymer = do
  let charFrequency = [M.singleton (x!!0) y | (x, y) <- M.toList polymer]
  let mergedCharFrequency = foldl (M.unionWith (+)) M.empty charFrequency
  [ y | (x, y) <- M.toList mergedCharFrequency]

input :: IO (Template, Rules)
input =  do
  [template, rawRules] <- map lines . splitOn "\n\n" <$> (readFile =<< getDataFileName "inputs/day14.txt")
  let rules =  M.fromList $ map createRule rawRules
  return (head template, rules)
  where
    createRule (x : y : ' ' : '-' : '>' : ' ' : z : []) = ([x, y], [[x, z], [z, y]])
  