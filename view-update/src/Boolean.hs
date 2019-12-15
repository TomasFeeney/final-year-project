{-# LANGUAGE TypeFamilies #-}

module Boolean
  ( Boolean'(..)
  , booleanGraph
  , getNodes
  , toInductive
  , booleanExpr
  , rev
  , sub
  ) where

import qualified Data.Graph.Inductive as G
import Data.Reify
import qualified Data.Text.Lazy as L
import System.IO.Unsafe

data Boolean'
  = And Boolean' Boolean'
  | Or Boolean' Boolean'
  | Not Boolean'
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

eval :: Boolean' -> Bool
eval (Lit b) = b
eval (And b1 b2) = (eval b1) && (eval b2)
eval (Or b1 b2) = (eval b1) || (eval b2)

booleanExpr :: Boolean'
booleanExpr = (Or (And (Var "x") (Var "x")) (And (Var "x") (Var "x")))

-- Given an output produce an input for the given expr to produce that output
--
data Input
  = Prim Bool
  | Any
  | None
  deriving (Show)

combineAny :: Bool -> Input -> Input -> Input
combineAny = undefined

combineAll :: Bool -> Input -> Input -> Input
combineAll = undefined

rev :: Boolean' -> Bool -> Input
rev (Var x) True = Prim True
rev (Var x) False = Prim False
rev (Lit b) x =
  if (b == x)
    then Any
    else None
rev (Not b) x = rev b (not x)
rev (And (b1) (b2)) x = combineAnd x res1 res2
  where
    res1 = rev b1 x
    res2 = rev b2 x
    combineAnd :: Bool -> Input -> Input -> Input
    combineAnd True None _ = None
    combineAnd True _ None = None
    combineAnd True (Prim True) (Prim False) = None
    combineAnd True (Prim False) (Prim True) = None
    combineAnd True Any y = y
    combineAnd True y Any = y
    combineAnd True i1 i2 = i1
    combineAnd False None None = None
    combineAnd False None y = y
    combineAnd False y None = y
    combineAnd False (Prim True) (Prim False) = Any
    combineAnd False (Prim False) (Prim True) = Any
    combineAnd False i1 i2 = i1
rev (Or (b1) (b2)) x = combineOr x res1 res2
  where
    res1 = rev b1 x
    res2 = rev b2 x
    combineOr :: Bool -> Input -> Input -> Input
    combineOr True None None = None
    combineOr True Any _ = Any
    combineOr True _ Any = Any
    combineOr True (Prim True) (Prim False) = Any
    combineOr True (Prim False) (Prim True) = Any
    combineOr True None x = x
    combineOr True x None = x
    combineOr True x y = y
    combineOr False None _ = None
    combineOr False _ None = None
    combineOr False Any Any = Any
    combineOr False (Prim True) (Prim False) = None
    combineOr False (Prim False) (Prim True) = None
    combineOr False i1 i2 = i1

sub :: Boolean' -> String -> Bool -> Boolean'
sub (Var x) y b =
  (if x == y
     then (Lit b)
     else (Var x))
sub (And b1 b2) y b = (And (sub b1 y b) (sub b2 y b))
sub (Or b1 b2) y b = (Or (sub b1 y b) (sub b2 y b))
sub (Not b1) y b = Not (sub b1 y b)
sub (Lit b) _ _ = (Lit b)
