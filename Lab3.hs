import Lab3Help
import Lab3GUI
import Graph
import Dijkstra

main :: IO ()
main = do
  Right bstops <- readStops "your-stops.txt"
  Right blines <- readLines "your-lines.txt"
  let graph = example -- Build your graph here using bstops and blines
  runGUI bstops blines graph shortestPath

parseGraph :: [BLineTable] -> Graph
parseGraph (x:xs) = undefined

