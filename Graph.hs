module Graph where

-- | Description of a Graph.
data Graph = Graph [Elem]
  deriving (Show)
data Elem  = Elem
  { namn   :: String
  , adj    :: [(String, Double)]
  } deriving (Eq,Ord,Show)

emptyGraph :: Graph
emptyGraph = Graph []

addName :: String -> Graph -> Graph
addName n (Graph []) = Graph [ Elem{namn = n, adj = []} ]
addName n (Graph es) = Graph ((Elem{namn = n, adj = []}) : es)

addAdj :: String -> String -> Double -> Graph -> Graph
addAdj _ _ _  (Graph [])
                = error "The graph is empty"
addAdj n n' d g = Graph ( l ++ [Elem {namn = name, adj = ((n',d):as)}] ++
                          r )
  where
  (Graph l,Elem{namn = name, adj = as},Graph r) = (findElem n g)

findElem :: String -> Graph -> (Graph,Elem,Graph)
findElem n (Graph []) = error "Not found"
findElem n (Graph (e:es))
  | getName e == n = (emptyGraph, e, Graph es)
  | otherwise      = (Graph (e : l), e', gr)
 where
 (Graph l,e',gr) = findElem n (Graph es)


getName :: Elem -> String
getName (Elem{namn = n}) = n

getAdj :: Elem -> [(String, Double)]
getAdj (Elem{adj = a})   = a

example :: Graph
example = Graph [ Elem { namn = "Brunnsparken"
                       , adj = [ ("Centralstationen",1)
                               , ("Lilla Bommen",2)
                               , ("Kungsportsplatsen",4)
                               , ("Domkyrkan",3) ] }
                , Elem { namn = "Centralstationen"
                       , adj = [ ("Brunnsparken",1) ] }
                , Elem { namn = "Lilla Bommen"
                       , adj = [ ("Brunnsparken",2) ] } 
                , Elem { namn = "Kungsportsplatsen"
                       , adj = [ ("Brunnsparken",4) ] }
                , Elem { namn = "Domkyrkan"
                       , adj = [ ("Brunnsparken",3) ] } ]
