data BDD = Leaf Bool | Node (Int, Int) BDD BDD
  deriving (Show)

trueBDD, falseBDD :: BDD
trueBDD = Leaf True
falseBDD = Leaf False

notBDD :: BDD -> BDD
notBDD (Leaf b) = Leaf (not b)
notBDD (Node var low high) = Node var (notBDD low) (notBDD high)

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

andList :: [BDD] -> BDD
andList = foldr andBDD trueBDD

orList :: [BDD] -> BDD
orList = foldr orBDD falseBDD

notList :: [BDD] -> BDD
notList = andList . map notBDD

pickOne :: [a] -> [(a, [a])]
pickOne [] = []
pickOne (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- pickOne xs ]

exactlyOne :: [BDD] -> BDD
exactlyOne vars = orList [ andBDD v (notList vs) | (v, vs) <- pickOne vars ]

atMostOne :: [BDD] -> BDD
atMostOne [] = trueBDD
atMostOne (v:vs) = andList [ notBDD (andBDD v v') | v' <- vs ] `andBDD` atMostOne vs

bddVar :: (Int, Int) -> BDD
bddVar pos = Node pos falseBDD trueBDD

-- ровно один ферщь в строке 
rowCheck :: Int -> Int -> BDD
rowCheck n i = exactlyOne [ bddVar (i, j) | j <- [0..n-1] ]

-- один ферзь в столбце
colCheck :: Int -> Int -> BDD
colCheck n j = exactlyOne [ bddVar (i, j) | i <- [0..n-1] ]

-- Один ферзь в диагонале 
diagCheck :: Int -> BDD
diagCheck n = andList $
  [ atMostOne [ bddVar (i,j) | i <- [0..n-1], j <- [0..n-1], i - j == d ]
  | d <- [-n+1..n-1] ] ++
  [ atMostOne [ bddVar (i,j) | i <- [0..n-1], j <- [0..n-1], i + j == d ]
  | d <- [0..2*n-2] ]

nQueensBDD :: Int -> BDD
nQueensBDD n = andList $
  [ rowCheck n i | i <- [0..n-1] ] ++
  [ colCheck n j | j <- [0..n-1] ] ++
  [ diagCheck n ]

main :: IO ()
main = do
  let bdd = nQueensBDD 4
  print bdd







-- bdd1 = Node (1,1) (Leaf True) (Node (1,2) (Leaf False) (Leaf True))
-- bdd2 = Node (0,1) (Leaf False) (Leaf True) 

-- testNotBDD = notBDD bdd1

-- testAndBDD = andBDD bdd1 bdd2

-- testOrBDD= orBDD bdd1 bdd2

-- main :: IO ()
-- main = do
--   putStrLn $ "notBDD" ++ show testNotBDD
--   putStrLn $ "andBDD" ++ show testAndBDD
--   putStrLn $ "orBDD" ++ show testOrBDD