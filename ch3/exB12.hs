-- File: ch3/exB12.hs
-- A function that computes the convex hull of a finite list of points.
import Data.List (groupBy, sortBy)

removeDuplicates :: Eq a => [a] -> [a]
-- ^ Why isn't this in Prelude?
removeDuplicates = foldr skipIfElem []
  where
    skipIfElem x ys = if x `elem` ys
                      then ys
                      else x : ys

-- | Type for encoding points in the Cartesian plane.
data Point = Point Double Double
             deriving (Eq, Read, Show)

slope :: Point -> Point -> Double
-- ^ Calculates the slope between two points.
slope (Point x1 y1) (Point x2 y2) = (y2 - y1) / (x2 - x1)

norm :: Point -> Point -> Double
-- ^ Calculates the taxicab distance between two points.
norm (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

coordinateSort :: [Point] -> [Point]
-- ^ Sorts by lowest x-coord (picking lowest y-coord in a tie)
coordinateSort = do
  sortBy (\(Point _ y1) (Point _ y2) -> compare y1 y2)
  sortBy (\(Point x1 _) (Point x2 _) -> compare x1 x2)

-- | Type for encoding relative direction.
data Direction = GoLeft | GoStraight | GoRight | GoBackwards | GoNowhere
                 deriving (Eq, Read, Show)

direction :: Point -> Point -> Point -> Direction
-- ^ Given points p1, p2, and p3, returns the direction to turn at `p2`
--   when traveling from `p1` through `p2` to `p3`.
direction p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) = do
  -- Check a few edge easy edge cases.
  if p1 == p2 || p2 == p3 -- direction is not well-defined
  then GoNowhere
  else if p1 == p3 -- direction is backwards
  then GoBackwards
  else do
    -- Here, we do a coordinate transformation that moves `p1` to the
    -- origin and moves `p2` to (1,0). This puts `p3'` somewhere in the
    -- plane, and then we branch on the trichotomy of `y3'`.
    let det = (x2 - x1) ** 2 + (y2 - y1) ** 2
        a   = (x2 - x1) / det
        b   = (y2 - y1) / det
        c   = - (y2 - y1)
        d   = x2 - x1
        x3' = a * (x3 - x1) + b * (y3 - y1)
        y3' = c * (x3 - x1) + d * (y3 - y1)
    if y3' > 0 -- `p3'` is in the upper half plane
    then GoLeft
    else if y3' < 0 -- `p3'` is in the lower half plane
    then GoRight
    else if x3' > 1 -- `p3'` is on the x-axis, beyond (1,0)
    then GoStraight
    else GoBackwards -- `p3'` is on the x-axis behind (1, 0)

grahamScan :: [Point] -> [Point]
grahamScan input = walkPerimeter . sort $ input
  where
    sort ps = (b :)
            . map (\(p,_,_) -> p)
            . map last
            . map (sortBy (\(_,_,n1) (_,_,n2) -> compare n1 n2))
            . groupBy (\(_,s1,_) (_,s2,_) -> s1 == s2)
            . sortBy (\(_,s1,_) (_,s2,_) -> compare s1 s2)
            . map (\p -> (p, slope b p, norm b p)) $ bs
      where
        (b : bs) = coordinateSort . removeDuplicates $ ps
    walkPerimeter (p1 : p2 : p3 : ps) =
      -- examine three points at a time
      if direction p1 p2 p3 == GoLeft
      then p1 : (walkPerimeter (p2 : p3 : ps)) -- GoLeft -> p1 is good
      else walkPerimeter (p1 : p3 : ps) -- not GoLeft -> p2 is bad
    walkPerimeter ps = ps -- base case, fewer than three points -> end
