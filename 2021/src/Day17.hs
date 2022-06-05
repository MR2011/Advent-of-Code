module Day17
  ( part1,
    part2,
  )
where

type Point = (Int, Int)

type Velocity = (Int, Int)

type TargetArea = ((Int, Int), (Int, Int))

part1 :: IO Int
part1 = do
  let (_, (yMin, _)) = input
  return $ (yMin + 1) * yMin `div` 2

part2 :: IO Int
part2 = do
  return $ length $ trajectoriesInBounds input

inTargetArea :: TargetArea -> Point -> Bool
inTargetArea ((xMin, xMax), (yMin, yMax)) (x, y) = xMin <= x && x <= xMax && yMin <= y && y <= yMax

simulate :: TargetArea -> Velocity -> Point -> [Point]
simulate targetArea (vx, vy) (x, y)
  | overshot targetArea (x, y) = []
  | otherwise = (x, y) : simulate targetArea (vx - signum vx, vy - 1) (x + vx, y + vy)

overshot :: TargetArea -> Point -> Bool
overshot ((xMin, xMax), (yMin, yMax)) (x, y) = x > xMax || y < yMin

trajectoriesInBounds :: TargetArea -> [[Point]]
trajectoriesInBounds targetArea@((xMin, xMax), (yMin, yMax)) = filter (any (inTargetArea targetArea)) [simulate targetArea (x, y) (0, 0) | x <- [0 .. xMax], y <- [yMin .. 1000]]

input :: TargetArea
input = ((156, 202), (-110, -69))