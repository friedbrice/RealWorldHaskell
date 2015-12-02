-- File: ch3/exB12.hs
-- A function that computes the convex hull of a finite list of points.
import Data.List (sortBy)

-- | Type for encoding relative direction.
data Direction = GoLeft | GoStraight | GoRight | GoBackwards | GoNowhere
                 deriving (Eq, Read, Show)

-- | Type for encoding points in the cartesian plane.
data Point = Point Double Double
             deriving (Eq, Read, Show)

xProj :: Point -> Double
xProj (Point x _) = x

yProj :: Point -> Double
yProj (Point _ y) = y

direction :: Point -> Point -> Point -> Direction
-- ^ Given points p1, p2, and p3, returns the direction to p3 when traveling
--   from p1 through p2.
--   Relative directions are hard.
--   After checking a few edge cases, we do a coordinate transformation
--   that makes the problem easy to check.
direction p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) = do
  if p1 == p2 || p2 == p3 -- Silly input.
  then GoNowhere
  else if p1 == p3 -- Silly input.
  then GoBackwards
  else do
    -- Here, we do a coordinate transformation that moves p1 to the
    -- origin and moves p2 to (1,0). This puts p3' somewhere in the
    -- plane, and we need to branch on the trichotomy of y3'.
    let det = x2 ** 2 + y2 ** 2
        a   = x2 / det
        b   = y2 / det
        c   = - y2
        d   = x2
        x3' = a * x3 + b * y3
        y3' = c * x3 + d * y3
    if y3' > 0 -- p3' is in the upper half plane.
    then GoLeft
    else if y3' < 0 -- p3' is in the lower half plane.
    then GoRight
    else if x3' > 1 -- p3' is on the x-axis, beyond (1,0).
    then GoStraight
    else GoBackwards -- p3' is on the x-axis behind (1, 0).

reverseDictSort :: [Point] -> [Point]
-- ^ Sorts by leftmostness (picking lowest in a tie)
reverseDictSort = do
  sortBy (\p1 p2 -> compare (yProj p1) (yProj p2))
  sortBy (\p1 p2 -> compare (xProj p1) (xProj p2))

slopeSort :: Point -> [Point] -> [Point]
-- ^ Sorts a list of points by the slope they form with the given point.
slopeSort (Point x y) = sortBy (\p1 p2 -> compare (slope p1) (slope p2))
  where slope p = (yProj p - y) / (xProj p - x)

grahamScan :: [Point] -> [Point]
-- ^ Returns the verticies of the convext hull of the input list.
grahamScan = do
  b <- head . reverseDictSort -- Find the leftmost point.
  (b :) . slopeSort b . tail -- Sort the other points by slope.
  walkPerimeter
  -- Take a walk along the list of points, throwing away inside points.
    where
      walkPerimeter (p1 : p2 : p3 : ps) =
        if direction p1 p2 p3 == GoLeft
        then p1 : (walkPerimeter (p2 : p3 : ps))
        else walkPerimeter (p1 : p3 : ps)
      walkPerimeter ps = ps
