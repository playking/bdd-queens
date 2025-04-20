data BDD = Leaf Bool | Node (Int, Int) BDD BDD
  deriving (Show)

trueBDD, falseBDD :: BDD
trueBDD = Leaf True
falseBDD = Leaf False

-- негатив 
notBDD:: BDD -> BDD
notBDD (Leaf b) = Leaf (not b)
notBDD (Node var low high) = Node var (notBDD low) (notBDD high)

-- конъюкция 
andBDD :: BDD -> BDD -> BDD
andBDD (Leaf True) bdd = bdd
andBDD (Leaf False) _  = falseBDD
andBDD bdd (Leaf True) = bdd
andBDD _ (Leaf False) = falseBDD
andBDD (Node v1 l1 h1) (Node v2 l2 h2)
  | v1 == v2  = Node v1 (andBDD l1 l2) (andBDD h1 h2)
  | v1 < v2   = Node v1 (andBDD l1 bdd2) (andBDD h1 bdd2)
  | otherwise = Node v2 (andBDD bdd1 l2) (andBDD bdd1 h2)
  where
    bdd1 = Node v1 l1 h1
    bdd2 = Node v2 l2 h2

orBDD :: BDD -> BDD -> BDD
orBDD a b = notBDD $ andBDD (notBDD a) (notBDD b)


bdd1 = Node (1,1) (Leaf True) (Node (1,2) (Leaf False) (Leaf True))
bdd2 = Node (0,1) (Leaf False) (Leaf True) 

testNotBDD = notBDD bdd1

testAndBDD = andBDD bdd1 bdd2

testOrBDD= orBDD bdd1 bdd2

main :: IO ()
main = do
  putStrLn $ "notBDD" ++ show testNotBDD
  putStrLn $ "andBDD" ++ show testAndBDD
  putStrLn $ "orBDD" ++ show testOrBDD