module Graph where

-- | Description of a Graph.
data Graph = Graph [Elem]
data Elem  = Elem
  { namn   :: String
  , pos    :: (Integer,Integer)
  , adj    :: [(String, Integer)]
  } deriving (Eq,Ord,Show)

getName :: Elem -> String
getName (Elem{namn = n}) = n

getPos :: Elem -> (Integer, Integer)
getPos (Elem{pos = p})   = p

getAdj :: Elem -> [(String, Integer)]
getAdj (Elem{adj = a})   = a