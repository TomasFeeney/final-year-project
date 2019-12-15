module Visualise
  ( testGraph
  , visualiseGraph
  , printGraph
  , walkGraph
  , writeGraphAsGif
  , writeTraversalAsGif
  ) where

import Data.Array
import Data.Graph.Inductive
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G
import qualified Data.Text.Lazy as L
import System.IO

testGraph :: Gr String String
testGraph = mkGraph (zip [1 .. 3] ["a", "b", "c"]) [(1, 2, "label")]

visualiseGraph :: Int -> Gr L.Text L.Text -> G.DotGraph Node
visualiseGraph n =
  G.graphToDot (G.nonClusteredParams {G.fmtNode = fn, G.fmtEdge = fe})
  where
    fe (_, _, l) = [G.textLabel l]
    fn (x, l) =
      [G.textLabel l] ++
      (if x == n
         then [G.color G.Red]
         else [])

printGraph :: Gr L.Text L.Text -> Int -> L.Text
printGraph g n = G.printDotGraph (visualiseGraph n g)

walkGraph :: Gr L.Text L.Text -> Int -> [Int] -> [L.Text]
walkGraph g n seen = [printGraph g n] ++ ns
  where
    ns :: [L.Text]
    ns =
      concat $
      (map
         (\x -> walkGraph g x (seen ++ [n] ++ [x]))
         (filter (not . (`elem` seen)) (neighbors g n)))

writeGraphAsGif :: Gr L.Text L.Text -> Int -> IO FilePath
writeGraphAsGif g n = do
  G.runGraphviz (visualiseGraph n g) (G.Gif) pathName
  where
    pathName = "out/out" ++ (show n) ++ ".gif"

writeTraversalAsGif :: Gr L.Text L.Text -> [IO FilePath]
writeTraversalAsGif g = walkAndWrite g 1 []
  where
    walkAndWrite :: Gr L.Text L.Text -> Int -> [Int] -> [IO FilePath]
    walkAndWrite g n seen = [writeGraphAsGif g n] ++ ns
      where
        ns :: [IO FilePath]
        ns =
          concat $
          (map
             (\x -> walkAndWrite g x (seen ++ [n] ++ [x]))
             (filter (not . (`elem` seen)) (neighbors g n)))
