{-# LANGUAGE FlexibleInstances #-}

module View
  ( Node
  , label
  , assign
  , labelMultiple
  ) where

import Control.Applicative
import qualified Data.Set as Set

type Identifier = Integer

data Node a =
  Node (a, Set.Set Identifier)
  deriving (Show, Eq)

{-data Tape a t = Literal a [Identifier]
            | Var String a [Identifier]
            | Binary a a t [Identifier]
            | Unary a t [Identifier]
            -}
instance Functor Node where
  fmap f (Node (x, ids)) = (Node ((f x), ids))

-- Given change spec for output return induced change spec.
instance Applicative Node where
  pure x = (Node (x, Set.empty))
  (<*>) (Node (f, ids)) (Node (a, aIds)) = (Node ((f a), (Set.union ids aIds)))

instance Monad Node where
  return x = pure x
  (>>=) (Node (a, aIds)) f = (Node (b, Set.union aIds bIds))
    where
      (Node (b, bIds)) = f a

--binary :: (a -> a -> b) -> a -> a -> (Tape a b)
instance (Num a) => Num (Node a) where
  (*) = liftA2 (*)
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = emptyNode . fromInteger

instance (Fractional a) => Fractional (Node a) where
  (/) = liftA2 (/)
  recip x = 1 / x
  fromRational = emptyNode . fromRational

emptyNode :: a -> (Node a)
emptyNode a = Node (a, Set.empty)

label :: Identifier -> a -> (Node a)
label id a = Node (a, Set.fromList [id])

labelMultiple :: [Identifier] -> a -> (Node a)
labelMultiple ids a = Node (a, Set.fromList ids)

assign :: [a] -> [Node a]
assign as = map (\(x, y) -> label y x) (zip as [1 ..])

example :: Fractional a => [a] -> [Node a]
example _xs = [(x * y) / x, z + z]
  where
    (x:y:z:_) = assign (_xs)
