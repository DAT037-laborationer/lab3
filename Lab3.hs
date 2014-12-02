import Lab3Help
import Lab3GUI

-- | Description of a Graph.
data Graph = Graph [Node] deriving (Show)

-- | Description of a Node.
data Node = Node 
  {  bsname     :: String	
  ,  blines     :: [Line]
  ,  next       :: [Node] 
  } deriving (Show)

type Line = (String,Int)

-- | Returns a list of stops along the shortest path between two nodes
-- | and the total travel time along that path.	
path :: Graph -> String -> String -> Maybe ([String], Int)
path g n1 n2 = undefined

infinity = 1/0

main :: IO()
main = putStr "HEEEJ"

parseGraph :: [BLineTable] -> Graph
parseGraph (x:xs) = undefined

times :: Node -> Node -> String
times n _ = n->bsname
