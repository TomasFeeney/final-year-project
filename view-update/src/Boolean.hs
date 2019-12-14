{-# LANGUAGE TypeFamilies #-}

module Boolean
  ( Boolean'(..)
  , booleanGraph
  , getNodes
  , toInductive
  ) where

import qualified Data.Graph.Inductive as G
import Data.Reify
import qualified Data.Text.Lazy as L
import System.IO.Unsafe

data Boolean'
  = And Boolean' Boolean'
  | Or Boolean' Boolean'
  | Lit Bool
  | Var String
  deriving (Show)

data BooleanNode s
  = GraphAnd s s
  | GraphOr s s
  | GraphLit Bool
  | GraphVar String
  deriving (Show)

instance MuRef Boolean' where
  type DeRef Boolean' = BooleanNode
  mapDeRef f (And a b) = GraphAnd <$> f a <*> f b
  mapDeRef f (Or a b) = GraphOr <$> f a <*> f b
  mapDeRef f (Lit b) = pure $ GraphLit b
  mapDeRef f (Var s) = pure $ GraphVar s

booleanGraph =
  toInductive (unsafePerformIO $ reifyGraph (And (Lit True) (Lit False)))

getNodes :: Graph BooleanNode -> [G.LNode L.Text]
getNodes (Graph xs _) = [(x, label y) | (x, y) <- xs]
  where
    label (GraphAnd _ _) = L.pack "And"
    label (GraphLit b) = L.pack $ show b
    label (GraphOr _ _) = L.pack "Or"
    label (GraphVar v) = L.pack "Var"

getEdges :: Graph BooleanNode -> [G.LEdge L.Text]
getEdges (Graph xs _) = concat ys
  where
    ys :: [[G.LEdge L.Text]]
    ys = map getEdgesFromNode xs

getEdgesFromNode :: (Int, BooleanNode Int) -> [G.LEdge L.Text]
getEdgesFromNode (a, (GraphAnd b c)) = [(a, b, L.pack ""), (a, c, L.pack "")]
getEdgesFromNode (a, (GraphOr b c)) = [(a, b, L.pack ""), (a, c, L.pack "")]
getEdgesFromNode (a, (GraphLit b)) = []
getEdgesFromNode (a, (GraphVar s)) = []

toInductive :: Graph BooleanNode -> G.Gr L.Text L.Text
toInductive g = G.mkGraph labelledNodes labelledEdges
  where
    labelledNodes = getNodes g
    labelledEdges = getEdges g
