{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day23
  ( part1,
    part2,
  )
where

import Algorithm.Search (dijkstra)
import qualified Data.Maybe
import Helpers (enumerate)

part1 :: IO Int
part1 = do
  let (cost, _) = Data.Maybe.fromMaybe (0, []) (solve ["CD", "CA", "BB", "DA"])
  return cost

part2 :: IO Int
part2 = do
  let (cost, _) = Data.Maybe.fromMaybe (0, []) (solve ["CDDD", "CCBA", "BBAB", "DACA"])
  return cost

{-
#01#2#3#4#56#
#..o.o.o.o..#
###A#B#C#D###
   0 1 2 3
-}

type Room = [Char]

type Rooms = [Room]

type Hallway = [Char]

type State = (Rooms, Hallway)

type Amphipod = Char

type Cost = Int

costMatrix :: [[Cost]]
costMatrix =
  [ [3, 2, 2, 4, 6, 8, 9],
    [5, 4, 2, 2, 4, 6, 7],
    [7, 6, 4, 2, 2, 4, 5],
    [9, 8, 6, 4, 2, 2, 3]
  ]

singleStepCost :: Amphipod -> Int
singleStepCost 'A' = 1
singleStepCost 'B' = 10
singleStepCost 'C' = 100
singleStepCost 'D' = 1000

roomFor :: Amphipod -> Int
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3

reachable :: Hallway -> Int -> Int -> Bool
reachable hall r h
  | r + 1 >= h = all (\i -> hall !! i == '.') [h .. r + 1]
  | otherwise = all (\i -> hall !! i == '.') [r + 2 .. h]

replace :: [a] -> Int -> a -> [a]
replace xs i c = [if j == i then c else x | (j, x) <- zip [0 ..] xs]

canMoveHome :: Room -> Char -> Bool
canMoveHome r a = all (== a) r

canReachHome :: Hallway -> Int -> Int -> Bool
canReachHome hallway h r
  | r + 1 >= h + 1 = all (\i -> openSpace (hallway !! i)) [h + 1 .. r + 1]
  | h - 1 >= r + 2 = all (\i -> openSpace (hallway !! i)) [r + 2 .. h -1]
  | otherwise = True

openSpace :: Char -> Bool
openSpace c = c == '.'

moveIntoHallway :: State -> [State]
moveIntoHallway (rooms, hallway) = [newState hallway j r i | (i, r) <- enumerate rooms, j <- [0 .. 6], notEmpty r && notDone r i && reachable hallway i j]
  where
    notEmpty r = not (null r)
    notDone r i = r /= replicate (length r) ("ABCD" !! i)
    newState h hIndex rs@(a : as) rIndex = (newRooms rIndex as, newHallway h hIndex a)
    newHallway hallway index amphipod = replace hallway index amphipod
    newRooms roomIndex amphipods = [if k == roomIndex then amphipods else rooms !! k | k <- [0 .. 3]]

moveIntoRoom :: State -> [State]
moveIntoRoom (rooms, hallway) = [newState h j | (j, h) <- enumerate hallway, not (openSpace h) && canMoveHome (room h) h && canReachHome hallway j (roomFor h)]
  where
    newState h hIndex = (newRooms h, newHallway hallway hIndex)
    newHallway hallway index = replace hallway index '.'
    room a = rooms !! roomFor a
    newRooms amphipod = replace rooms (roomFor amphipod) (amphipod : room amphipod)

neighboringStates :: State -> [State]
neighboringStates state = moveIntoHallway state ++ moveIntoRoom state

costMove :: Int -> State -> Int -> Int -> Cost
costMove len (rooms, hall) roomIndex hallwayIndex = (costMatrix !! roomIndex !! hallwayIndex + offset) * singleStepCost a
  where
    as = rooms !! roomIndex
    a = if openSpace (hall !! hallwayIndex) then head as else hall !! hallwayIndex
    offset = if openSpace (hall !! hallwayIndex) then len - length as else len - 1 - length as

costTransition :: Int -> State -> State -> Cost
costTransition len (rs, h) (rs', h') = costMove len (rs, h) roomIndex hallwayIndex
  where
    roomIndex = firstDifference rs rs'
    hallwayIndex = firstDifference h h'

firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0 .. length xs], xs !! i /= ys !! i]

solutionFound :: Int -> State -> Bool
solutionFound len state = fst state == map (replicate len) "ABCD"

solve :: Rooms -> Maybe (Cost, [State])
solve rooms = dijkstra neighboringStates (costTransition l) (solutionFound l) (rooms, ".......")
  where
    l = length $ head rooms