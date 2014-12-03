module Dijkstra where

import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as PSQ
import Graph

type Vertex = String
type Weight = Graph -> Vertex -> Vertex -> Double
type Cost   = Double

-- | Givnen a binding, decrease lowers a vertex's path cost in a priority 
-- | search queue.
decrease :: (Ord k, Ord p) => Binding k p -> PSQ k p -> PSQ k p
decrease (k :-> p) q = PSQ.adjust (min p) k q

-- | Given a list of bindings, decreaseList lowers the vertices' path costs
-- | in a priority search queue
decreaseList :: (Ord k, Ord p) => [Binding k p] -> PSQ k p -> PSQ k p
decreaseList bs q = foldr decrease q bs

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
vertices (Graph ns) = [ getName x | x <- ns ]
 
-- | Takes a graph and two nodes and returns a list of stops along the shortest
-- | path between two nodes and the total travel time along that path.	
dijkstra :: Graph -> Vertex -> Vertex -> Maybe ([Vertex], Cost)
dijkstra g u v = loop (decrease (u :-> 0) q0)
  where
  q0 = PSQ.fromAscList [ v :-> infinity | v <- vertices g ]
  loop q = case PSQ.minView q of
    Nothing           -> Nothing
    Just (w :-> d, q) -> case v == w of
      True -> Just ([], 0)
      _    -> Just (v:vs, weight g u w + c)
        where Just (vs,c) = loop (decreaseList bs q)
              bs = [v :-> d + weight g u v | v <- adjacent g u ]
