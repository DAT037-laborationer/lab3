module Graph where

-- | Description of a Graph.
data Graph = Graph [Elem]
  deriving (Show)
data Elem  = Elem
  { namn   :: String
  , pos    :: (Integer,Integer)
  , adj    :: [(String, Double)]
  } deriving (Eq,Ord,Show)

getName :: Elem -> String
getName (Elem{namn = n}) = n

getPos :: Elem -> (Integer, Integer)
getPos (Elem{pos = p})   = p

getAdj :: Elem -> [(String, Double)]
getAdj (Elem{adj = a})   = a

example :: Graph
example = Graph [ Elem { namn = "Brunnsparken"
                       , pos = (0,0)
                       , adj = [ ("Centralstationen",1)
                               , ("Lilla Bommen",2)
                               , ("Kungsportsplatsen",4)
                               , ("Domkyrkan",3) ] }
                , Elem { namn = "Centralstationen"
                       , pos = (0,0)
                       , adj = [ ("Brunnsparken",1) ] }
                , Elem { namn = "Lilla Bommen"
                       , pos = (0,0)
                       , adj = [ ("Brunnsparken",2) ] } 
                , Elem { namn = "Kungsportsplatsen"
                       , pos = (0,0)
                       , adj = [ ("Brunnsparken",4) ] }
                , Elem { namn = "Domkyrkan"
                       , pos = (0,0)
                       , adj = [ ("Brunnsparken",3) ] } ]