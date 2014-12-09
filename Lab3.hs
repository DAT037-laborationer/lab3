import System.Environment
import Lab3Help
import Lab3GUI
import Graph
import Dijkstra

main :: IO ()
main = do
  a <- getArgs
  if length a < 2 then error "Too few arguments given" else putStr "Hej"
  Right bstops <- readStops (a !! 0)
  Right blines <- readLines (a !! 1)
  let graph = toGraph bstops blines -- Build your graph here using bstops and blines
  print graph
--  runGUI bstops blines graph dijkstra

toGraph :: [BStop] -> [BLineTable] -> Graph
toGraph bs bls = addNextStops bls (addStops bs emptyGraph)

addStops :: [BStop] -> Graph -> Graph
addStops [] g                     = g
addStops ((BStop{name = n}):bs) g = addStops bs (addName n g)

addNextStops :: [BLineTable] -> Graph -> Graph
addNextStops [] g = g
addNextStops ((BLineTable {stops = (BLineStop{stopName = s1}:BLineStop{stopName = s2, time = t}:ss)}) : bls) g
                  = addNextStops (bls) (addAdj s1 s2 (fromIntegral t) g)

