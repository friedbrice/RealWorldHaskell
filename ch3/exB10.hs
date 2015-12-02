-- File: ch3/exB10.hs
-- A function that takes three points---p1, p2, and p3---and determines the
-- direction needed to turn at p2 in order to continue from p1 to p3.

data Direction = GoLeft | GoStraight | GoRight | GoBackwards | GoNowhere
                 deriving (Eq, Read, Show)

data Point = Point Double Double
             deriving (Eq, Read, Show)

directions :: Point -> Point -> Point -> Direction
-- ^ Relative directions are hard.
--   After checking a few edge cases, we do a coordinate transformation
--   that makes the problem easy to check.
directions p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) = do
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
