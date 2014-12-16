import System.Environment
import Lab3Help
import Lab3GUI
import Graph
import Dijkstra

-- | The main program reads two text files, and converts them to a graph. 
-- | A graphical user interface is then shown in the browser, where the
-- | user can find the shortest path from two stops.
main :: IO ()
main = do
  a <- getArgs
  if length a < 2 then error "Too few arguments given" else doNothing
  Right bstops <- readStops (a !! 0)
  Right blines <- readLines (a !! 1)
  let graph = toGraph bstops blines -- Build your graph here using bstops and blines
  runGUI bstops blines graph dijkstra

-- | Does nothing.
doNothing :: IO()
doNothing = return ()

-- | Takes two lists of BStops and BLineTables and converts them to a 
-- | Graph.
toGraph :: [BStop] -> [BLineTable] -> Graph
toGraph bs bls = addNextStops bls (addStops bs emptyGraph)

-- | Adds BStops to a Graph.
addStops :: [BStop] -> Graph -> Graph
addStops [] g                     = g
addStops (b:bs) g = addStops bs (addName (name b) g)

-- | Adds adjacent stops to a Graph.                  
addNextStops :: [BLineTable] -> Graph -> Graph
addNextStops [] g = g
addNextStops (bl:bls) g = addNextStops bls (func bl g)
  where
  func :: BLineTable -> Graph -> Graph
  func bl g
    | xs == []  = adj
    | otherwise = func bl' adj
    where 
    a : b : xs = stops bl
    bl'        = BLineTable {lineNumber = lineNumber bl, stops = (b:xs)}
    adj        = addAdj (stopName a) (stopName b) ((fromIntegral . time) b) g


