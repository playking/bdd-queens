import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.GraphViz
import Data.GraphViz.Attributes (toLabel)
import qualified Data.Text.Lazy.IO as T
import Data.GraphViz.Printing (renderDot, toDot)
import System.IO (hFlush, stdout)
import Data.GraphViz.Attributes.Complete



data BDD = Leaf Bool | Node (Int, Int) BDD BDD deriving Show

trueBDD, falseBDD :: BDD
trueBDD = Leaf True
falseBDD = Leaf False

notBDD :: BDD -> BDD
notBDD (Leaf b) = Leaf (not b)
notBDD (Node var l h) = Node var (notBDD l) (notBDD h)

andBDD :: BDD -> BDD -> BDD
andBDD (Leaf True) b = b
andBDD (Leaf False) _ = falseBDD
andBDD b (Leaf True) = b
andBDD _ (Leaf False) = falseBDD
andBDD (Node v1 l1 h1) (Node v2 l2 h2)
  | v1 == v2  = Node v1 (andBDD l1 l2) (andBDD h1 h2)
  | v1 < v2   = Node v1 (andBDD l1 b2) (andBDD h1 b2)
  | otherwise = Node v2 (andBDD b1 l2) (andBDD b1 h2)
  where
    b1 = Node v1 l1 h1
    b2 = Node v2 l2 h2

orBDD :: BDD -> BDD -> BDD
orBDD a b = notBDD (andBDD (notBDD a) (notBDD b))

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
bddVar (i, j) = Node (i+1, j+1) falseBDD trueBDD

rowCheck :: Int -> Int -> BDD
rowCheck n i = exactlyOne [ bddVar (i, j) | j <- [0..n-1] ]

colCheck :: Int -> Int -> BDD
colCheck n j = exactlyOne [ bddVar (i, j) | i <- [0..n-1] ]

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


type NodeId = Int
-- type Label = String

bddToGraph :: BDD -> G.Gr String String
bddToGraph bdd = G.mkGraph nodes edges
  where
    (nodes, edges, _) = build bdd 0

    build :: BDD -> NodeId -> ([(NodeId, String)], [(NodeId, NodeId, String)], NodeId)
    build (Leaf b) nid = ([(nid, show b)], [], nid + 1)
    build (Node (i, j) low high) nid =
      let currLabel = "x" ++ show i ++ "_" ++ show j
          (lowNodes, lowEdges, lowNext) = build low (nid + 1)
          (highNodes, highEdges, highNext) = build high lowNext
          nodes = (nid, currLabel) : lowNodes ++ highNodes
          edges = (nid, nid + 1, "0") : (nid, lowNext, "1") : lowEdges ++ highEdges
       in (nodes, edges, highNext)

exportBDD :: FilePath -> BDD -> IO ()
exportBDD path bdd = do
  let graph = bddToGraph bdd
      dotGraph = graphToDot nonClusteredParams {
        fmtNode = \(n, l) ->
          let base = [toLabel l]
              style = case l of
                "True"  -> [fillColor Green, Style [filled]]
                "False" -> [fillColor Red, Style [filled]]
                _       -> [fillColor LightBlue, Style [filled], shape BoxShape]
          in style ++ base,
        fmtEdge = \(_,_,l) -> [toLabel l]
      } graph

  T.writeFile path (renderDot $ toDot dotGraph)


main :: IO ()
main = do
  putStr "Enter the number of queens (n): "
  hFlush stdout 
  input <- getLine
  let n = read input :: Int
      outputFile = "queens_" ++ show n ++ "x" ++ show n ++ ".png"
  
  let bdd = nQueensBDD n
  exportBDD "nqueens.dot" bdd
  putStrLn "DOT file written to nqueens.dot"
  
  let cmd = "dot -Tpng nqueens.dot -o " ++ outputFile
  putStrLn $ "in cmd: " ++ cmd
  
