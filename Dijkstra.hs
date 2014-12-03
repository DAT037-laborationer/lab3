module Dijkstra where

import Data.PSQueue
import Graph

type Vertex = String
type Weight = Graph -> Vertex -> Vertex -> Integer
type Cost   = Integer

weight :: Weight
weight (Graph []) _ _ = error "Not found"
weight (Graph (e:es)) u v
  | getName e == u    = findV (getAdj e) v 
  | otherwise         = weight (Graph es) u v
  where findV [] _ = error "Not found"
        findV ((n,w):as) v
          | n == v    = w
          | otherwise = findV as v

infinity = 1/0

adjacent :: Graph -> Vertex -> [Vertex]
adjacent (Graph []) _ = []
adjacent (Graph (e:es)) v 
  | getName e == v    = map (fst) (getAdj e)
  | otherwise         = adjacent (Graph es) v

vertices :: Graph -> [Vertex]
vertices (Graph n) = [ getName x | x <- n]
 
-- | Takes a graph and two nodes and returns a list of stops along the shortest
-- |  path between two nodes and the total travel time along that path.	
shortestPath :: Graph -> Vertex -> Vertex -> Maybe ([Vertex], Cost)
shortestPath g from to = undefined