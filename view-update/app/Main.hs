module Main where

import Boolean
import qualified Data.Graph.Inductive as D
import qualified Data.GraphViz.Commands as G
import qualified Data.Text.Lazy as L
import System.IO
import Visualise

main :: IO ()
-- main = mapM_ (writeGraphAsGif) (zip (walkGraph booleanGraph 1 []) [1 ..])
main = putStrLn "hey"{-
writeGraph :: (L.Text, Int) -> IO ()
writeGraph (g, n) = do
  writeFile pathName (L.unpack g)
  return ()
  where
    pathName = "../out/out" ++ (show n) ++ ".viz"

-- writeGraphAsGif :: (D.Gr L.Text L.Text, Int) -> IO Bool
writeGraphAsGif (g, n) = G.runGraphviz (visualiseGraph n g) (G.Gif) pathName
  where
    pathName = "../out/out" ++ (show n) ++ ".gif"
-}
