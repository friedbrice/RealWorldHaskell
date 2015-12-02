-- File: ch3/exA2.hs
-- A tree struct that uses Maybe to represent leaves.

data MTree a = Node a (Maybe (MTree a)) (Maybe (MTree a))
			   deriving (Show)
