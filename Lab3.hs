-- import Lab3Help
-- import Lab3GUI
import PriorityQueue
import Data.Char

-- | Description of a Graph.
type Graph = BinHeap Elem
data Elem  = Elem
  { name :: String
  , cost :: Integer
  , pos  :: (Integer,Integer)
  , adj  :: [String]
  } deriving (Show)

{-
-- | Description of a Node.
data Node = Node 
  {  bsname     :: String	
  ,  blines     :: [Line]
  ,  next       :: [Node] 
  } deriving (Show)
-}

type Line = (String,Int)

-- | Returns a list of stops along the shortest path between two nodes
-- | and the total travel time along that path.	
path :: Graph -> String -> String -> Maybe ([String], Int)
path g n1 n2 = undefined

infinity = 1/0

--main :: IO ()
--main = do
--  Right bstops <- readStops "your-stops.txt"
--  Right blines <- readLines "your-lines.txt"
--  let graph = undefined -- Build your graph here using bstops and blines
--  runGUI bstops blines graph shortestPath

-- parseGraph :: [BLineTable] -> Graph
-- parseGraph (x:xs) = undefined

-- times :: Node -> Node -> String
-- times n _ = n->bsname

test :: String -> Int -> IO ()
test s n = putStrLn [ if (n <= ord 'z' - ord c && c >= 'a' && c <= 'z')
                        || (n <= ord 'Z' - ord c && c >= 'A' && c <= 'Z')
                      then chr (ord c + n)
                      else if isAlpha c
                           then chr (ord c + n - 26)
                           else c
                    | c <- s ]
