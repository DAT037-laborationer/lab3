import Lab3Help
import Lab3GUI
import Graph
import Dijkstra



type Line = (String,Int)

{-
-- | Description of a Node.
data Node = Node 
  {  bsname     :: String	
  ,  blines     :: [Line]
  ,  next       :: [Node] 
  } deriving (Show)
-}




{-
main :: IO ()
main = do
  Right bstops <- readStops "your-stops.txt"
  Right blines <- readLines "your-lines.txt"
  let graph = undefined -- Build your graph here using bstops and blines
  runGUI bstops blines graph shortestPath

parseGraph :: [BLineTable] -> Graph
parseGraph (x:xs) = undefined

-}
